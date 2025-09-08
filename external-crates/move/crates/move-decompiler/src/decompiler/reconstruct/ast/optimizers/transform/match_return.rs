use crate::decompiler::reconstruct::{DecompiledCodeItem, DecompiledCodeUnit, ResultUsageType};

pub(crate) fn rewrite_match_return(unit: &mut DecompiledCodeUnit) -> Result<(), anyhow::Error> {
    for item in unit.blocks.iter_mut() {
        match item {
            DecompiledCodeItem::IfElseStatement {
                if_unit, else_unit, ..
            } => {
                rewrite_match_return(if_unit)?;
                rewrite_match_return(else_unit)?;
            }

            DecompiledCodeItem::VariantSwitchStatement {
                cases,
                use_as_result,
                ..
            } => {
                if cases.len() == 0 {
                    continue;
                }
                for case in cases.iter_mut() {
                    rewrite_match_return(case)?;
                }
                let mut is_all_terminated = true;
                let mut has_return = false;
                let mut has_abort = false;
                for case in cases.iter_mut() {
                    let r = is_terminated(case, true);
                    if !r.0 {
                        is_all_terminated = false;
                    }
                    if r.1 {
                        has_return = true;
                    }
                    if r.2 {
                        has_abort = true;
                    }
                }
                if !is_all_terminated {
                    continue;
                }

                if has_return && use_as_result != &ResultUsageType::Abort {
                    // move the return to outside of the match
                    *use_as_result = ResultUsageType::Return;
                    for case in cases.iter_mut() {
                        update_block_return_to_exit(case)?;
                    }
                } else if has_abort && use_as_result != &ResultUsageType::Return {
                    // move the abort to outside of the match
                    *use_as_result = ResultUsageType::Abort;
                    for case in cases.iter_mut() {
                        update_block_abort_to_exit(case)?;
                    }
                } else if has_return || has_abort {
                    unreachable!();
                }
            }

            DecompiledCodeItem::WhileStatement { body, .. } => {
                rewrite_match_return(body)?;
            }

            _ => {}
        }
    }

    Ok(())
}

fn update_block_abort_to_exit(case: &mut DecompiledCodeUnit) -> Result<(), anyhow::Error> {
    assert!(
        case.exit.is_none() && case.result_variables.len() == 0,
        "Cannot update block abort to exit if exit is already set"
    );
    let exit = if let Some(DecompiledCodeItem::AbortStatement(expr)) = case.blocks.last().clone() {
        Some(expr.clone())
    } else {
        None
    };
    if let Some(exit) = exit {
        case.blocks.pop();
        case.exit = Some(exit);
    }
    Ok(())
}

fn update_block_return_to_exit(case: &mut DecompiledCodeUnit) -> Result<(), anyhow::Error> {
    assert!(
        case.exit.is_none() && case.result_variables.len() == 0,
        "Cannot update block return to exit if exit is already set"
    );
    let exit = if let Some(DecompiledCodeItem::ReturnStatement(expr)) = case.blocks.last() {
        Some(expr.clone())
    } else {
        None
    };
    if let Some(exit) = exit {
        case.blocks.pop();
        case.exit = Some(exit);
    }
    Ok(())
}

/**
 * Return (is_terminated, has_return, has_abort)
 */
fn is_terminated(unit: &DecompiledCodeUnit, allow_loop_termination: bool) -> (bool, bool, bool) {
    let mut is_terminated_result = false;
    let mut has_return = false;
    let mut has_abort = false;
    for item in unit.blocks.iter() {
        match item {
            DecompiledCodeItem::ReturnStatement { .. } => {
                is_terminated_result = true;
                has_return = true;
            }
            DecompiledCodeItem::AbortStatement { .. } => {
                is_terminated_result = true;
                has_abort = true;
            }
            DecompiledCodeItem::BreakStatement { .. }
            | DecompiledCodeItem::ContinueStatement { .. } => {
                if allow_loop_termination {
                    is_terminated_result = true;
                }
            }
            DecompiledCodeItem::IfElseStatement {
                if_unit,
                else_unit,
                use_as_result,
                ..
            } => {
                let r1 = is_terminated(if_unit, allow_loop_termination);
                let r2 = is_terminated(else_unit, allow_loop_termination);
                if matches!(
                    use_as_result,
                    ResultUsageType::Return | ResultUsageType::Abort
                ) || (r1.0 && r2.0)
                {
                    is_terminated_result = true;
                }
                if use_as_result == &ResultUsageType::Return || r1.1 || r2.1 {
                    has_return = true;
                }
                if use_as_result == &ResultUsageType::Abort || r1.2 || r2.2 {
                    has_abort = true;
                }
            }
            DecompiledCodeItem::WhileStatement { .. } => {
                // ignore loop as return values cannot escape from loop
            }
            DecompiledCodeItem::VariantSwitchStatement {
                cases,
                use_as_result,
                ..
            } => {
                if cases.len() > 0 {
                    let mut cases_terminated = true;
                    for case in cases.iter() {
                        let r = is_terminated(case, allow_loop_termination);
                        if !r.0 {
                            cases_terminated = false;
                        }
                        if r.1 {
                            has_return = true;
                        }
                        if r.2 {
                            has_abort = true;
                        }
                    }
                    if cases_terminated {
                        is_terminated_result = true;
                    }
                }
                match use_as_result {
                    ResultUsageType::None => {}
                    ResultUsageType::BlockResult => {}
                    ResultUsageType::Return => {
                        is_terminated_result = true;
                        has_return = true;
                    }
                    ResultUsageType::Abort => {
                        is_terminated_result = true;
                        has_abort = true;
                    }
                }
            }
            DecompiledCodeItem::CommentStatement(_)
            | DecompiledCodeItem::PossibleAssignStatement { .. }
            | DecompiledCodeItem::PreDeclareStatement { .. }
            | DecompiledCodeItem::AssignStatement { .. }
            | DecompiledCodeItem::AssignTupleStatement { .. }
            | DecompiledCodeItem::AssignStructureStatement { .. }
            | DecompiledCodeItem::Statement { .. } => {}
        }
    }
    (is_terminated_result, has_return, has_abort)
}
