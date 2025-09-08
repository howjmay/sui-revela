// Copyright (c) Verichains, 2023

use std::collections::HashSet;

use move_model::ty::Type;

use self::expr::{DecompiledExpr, DecompiledExprRef};

use super::super::naming::Naming;

use super::code_unit::SourceCodeUnit;

pub mod expr;
pub mod optimizers;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ResultUsageType {
    None,
    Return,
    Abort,
    BlockResult,
}

#[derive(Debug, Clone, PartialEq)]
enum InTerminationType {
    None,
    Return,
    Abort,
}

#[derive(Debug, Clone)]
pub(crate) enum DecompiledCodeItem {
    ReturnStatement(DecompiledExprRef),
    AbortStatement(DecompiledExprRef),
    BreakStatement,
    ContinueStatement,
    CommentStatement(String),
    PossibleAssignStatement {
        #[allow(dead_code)]
        assignment_id: usize,
        variable: usize,
        value: DecompiledExprRef,
        is_decl: bool,
    },
    PreDeclareStatement {
        variable: usize,
    },
    AssignStatement {
        variable: usize,
        value: DecompiledExprRef,
        is_decl: bool,
    },
    AssignTupleStatement {
        variables: Vec<usize>,
        value: DecompiledExprRef,
        is_decl: bool,
    },
    AssignStructureStatement {
        structure_visible_name: String,
        variables: Vec<(String, usize)>,
        fields_count: usize,
        value: DecompiledExprRef,
        ref_type: Option<move_model::model::RefType>,
    },
    Statement {
        expr: DecompiledExprRef,
    },
    IfElseStatement {
        cond: DecompiledExprRef,
        if_unit: DecompiledCodeUnitRef,
        else_unit: DecompiledCodeUnitRef,
        result_variables: Vec<usize>,
        /* this if statement is used as return value */
        use_as_result: ResultUsageType,
    },
    WhileStatement {
        cond: Option<DecompiledExprRef>,
        body: DecompiledCodeUnitRef,
    },
    VariantSwitchStatement {
        expr: DecompiledExprRef,
        cases_ref_type: Option<move_model::model::RefType>,
        local_variable: usize,
        cases: Vec<DecompiledCodeUnitRef>,
        result_variables: Vec<usize>,
        /* this if statement is used as return value */
        use_as_result: ResultUsageType,
    },
}

pub(crate) type DecompiledCodeUnitRef = Box<DecompiledCodeUnit>;

#[derive(Debug, Clone)]
pub(crate) struct CaseVariableDescription {
    variable: usize,
    variant_index: usize,
    enum_type: Type,
    default_match_case_expr: String,
}

impl CaseVariableDescription {
    fn clone_with_variable_remap(&self, f: &dyn Fn(usize) -> usize) -> Self {
        Self {
            variable: f(self.variable),
            variant_index: self.variant_index,
            enum_type: self.enum_type.clone(),
            default_match_case_expr: self.default_match_case_expr.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct DecompiledCodeUnit {
    pub(crate) blocks: Vec<DecompiledCodeItem>,
    pub(crate) exit: Option<DecompiledExprRef>,
    // sorted by variable index
    pub(crate) result_variables: Vec<usize>,
    pub(crate) for_case_variable: Option<CaseVariableDescription>,
}

impl DecompiledCodeUnit {
    pub fn new() -> DecompiledCodeUnitRef {
        Box::new(DecompiledCodeUnit {
            blocks: Vec::new(),
            exit: None,
            result_variables: Vec::new(),
            for_case_variable: None,
        })
    }

    pub fn new_empty(&self) -> DecompiledCodeUnitRef {
        Box::new(DecompiledCodeUnit {
            blocks: Vec::new(),
            exit: None,
            result_variables: Vec::new(),
            for_case_variable: self.for_case_variable.clone(),
        })
    }

    pub fn set_for_case_variable(
        &mut self,
        variable: usize,
        variant_index: usize,
        enum_type: Type,
        default_match_case_expr: String,
    ) {
        self.for_case_variable = Some(CaseVariableDescription {
            variable,
            variant_index,
            enum_type,
            default_match_case_expr,
        });
    }

    pub fn extends(&mut self, other: DecompiledCodeUnitRef) -> Result<(), anyhow::Error> {
        self.extends_main(other, true)
    }

    pub fn extends_main(
        &mut self,
        other: DecompiledCodeUnitRef,
        copy_result_variables: bool,
    ) -> Result<(), anyhow::Error> {
        if self.exit.is_some() {
            return Err(anyhow::anyhow!("Cannot extend terminated code unit"));
        }

        self.blocks.extend(other.blocks);

        if other.exit.is_some() {
            self.exit = other.exit;
            if copy_result_variables {
                self.result_variables = other.result_variables;
            }
        }

        Ok(())
    }

    pub fn add(&mut self, item: DecompiledCodeItem) {
        self.blocks.push(item);
    }

    pub fn exit(
        &mut self,
        variables: Vec<usize>,
        expr: DecompiledExprRef,
        must_be_preempty: bool,
    ) -> Result<(), anyhow::Error> {
        if let Some(current) = &self.exit {
            if must_be_preempty && current != &expr {
                return Err(anyhow::anyhow!("Cannot set exit expr twice"));
            }
        }

        self.result_variables = variables;
        self.exit = Some(expr);

        Ok(())
    }

    pub fn has_reference_to_any_variable(&self, variables: &HashSet<usize>) -> bool {
        for block in &self.blocks {
            match block {
                DecompiledCodeItem::PossibleAssignStatement {
                    variable, value, ..
                } => {
                    if variables.contains(variable)
                        || value.has_reference_to_any_variable(variables)
                    {
                        return true;
                    }
                }

                DecompiledCodeItem::PreDeclareStatement { variable } => {
                    if variables.contains(variable) {
                        return true;
                    }
                }

                DecompiledCodeItem::ReturnStatement(expr)
                | DecompiledCodeItem::AbortStatement(expr)
                | DecompiledCodeItem::Statement { expr } => {
                    if expr.has_reference_to_any_variable(variables) {
                        return true;
                    }
                }

                DecompiledCodeItem::BreakStatement
                | DecompiledCodeItem::ContinueStatement
                | DecompiledCodeItem::CommentStatement(_) => {}
                DecompiledCodeItem::AssignStatement {
                    variable, value, ..
                } => {
                    if variables.contains(variable)
                        || value.has_reference_to_any_variable(variables)
                    {
                        return true;
                    }
                }

                DecompiledCodeItem::AssignTupleStatement {
                    variables: vs,
                    value,
                    ..
                } => {
                    if vs.iter().any(|v| variables.contains(v))
                        || value.has_reference_to_any_variable(variables)
                    {
                        return true;
                    }
                }

                DecompiledCodeItem::AssignStructureStatement {
                    variables: vs,
                    value,
                    ..
                } => {
                    if vs.iter().any(|(_, v)| variables.contains(v))
                        || value.has_reference_to_any_variable(variables)
                    {
                        return true;
                    }
                }

                DecompiledCodeItem::IfElseStatement {
                    cond,
                    if_unit,
                    else_unit,
                    ..
                } => {
                    if cond.has_reference_to_any_variable(variables)
                        || if_unit.has_reference_to_any_variable(variables)
                        || else_unit.has_reference_to_any_variable(variables)
                    {
                        return true;
                    }
                }

                DecompiledCodeItem::WhileStatement { cond, body } => {
                    if cond
                        .as_ref()
                        .map(|x| x.has_reference_to_any_variable(variables))
                        .unwrap_or(false)
                        || body.has_reference_to_any_variable(variables)
                    {
                        return true;
                    }
                }

                DecompiledCodeItem::VariantSwitchStatement { cases, .. } => {
                    if cases
                        .iter()
                        .any(|c| c.has_reference_to_any_variable(variables))
                    {
                        return true;
                    }
                }
            }
        }

        false
    }

    pub fn to_source(
        &self,
        naming: &Naming,
        root_block: bool,
        mutable_variables: &HashSet<usize>,
    ) -> Result<SourceCodeUnit, anyhow::Error> {
        let mut finalize_wrapper = false;
        let mut wrapper_source = SourceCodeUnit::new(0);
        let mut source = SourceCodeUnit::new(0);
        let mut iter = self.blocks.iter().peekable();

        let mut first = true;

        while let Some(item) = iter.next() {
            if first {
                first = false;
                if let Some(v) = self.for_case_variable.as_ref() {
                    if let DecompiledCodeItem::AssignStructureStatement {
                        structure_visible_name,
                        fields_count,
                        variables,
                        value,
                        ref_type,
                    } = item
                    {
                        if value.is_single_variable_expr() != Some(v.variable) {
                            panic!("Invariant Exception: First item of code unit must be AssignStructureStatement with value being match variable")
                        }
                        let next_item = iter.peek();
                        let no_variable_referenced = variables
                            .iter()
                            .all(|(_, v)| !naming.is_variable_referenced(*v));
                        let (variables, _ref_type) = if let (
                            true,
                            Some(DecompiledCodeItem::AssignStructureStatement {
                                structure_visible_name: next_name,
                                fields_count: next_fields_count,
                                variables: next_variables,
                                value: _,
                                ref_type: next_ref_type,
                            }),
                        ) = (no_variable_referenced, next_item)
                        {
                            if next_name == structure_visible_name
                                && fields_count == next_fields_count
                            {
                                iter.next();
                                (next_variables, next_ref_type)
                            } else {
                                (variables, ref_type)
                            }
                        } else {
                            (variables, ref_type)
                        };
                        // again, as the variables may be updated
                        if *fields_count > 0 {
                            let no_variable_referenced = variables
                                .iter()
                                .all(|(_, v)| !naming.is_variable_referenced(*v));
                            if no_variable_referenced {
                                wrapper_source
                                    .add_line(format!("{} {{ .. }} => {{", structure_visible_name));
                            } else {
                                wrapper_source.add_line(format!(
                                    "{} {{ {}{} }} => {{",
                                    structure_visible_name,
                                    variables
                                        .iter()
                                        .map(|(k, v)| format!(
                                            "{}: {}{}",
                                            k,
                                            match mutable_variables.contains(v) {
                                                true => "mut ",
                                                false => "",
                                            },
                                            naming.variable(*v)
                                        ))
                                        .collect::<Vec<_>>()
                                        .join(", "),
                                    if *fields_count > variables.len() {
                                        ", .."
                                    } else {
                                        ""
                                    }
                                ));
                            }
                        } else {
                            wrapper_source.add_line(format!("{} => {{", structure_visible_name));
                        }
                    } else {
                        wrapper_source.add_line(format!("{} => {{", v.default_match_case_expr));
                    }
                    finalize_wrapper = true;
                    source.add_indent(1);
                    continue;
                }
            }
            let can_obmit_return = root_block && iter.peek().is_none() && self.exit.is_none();
            match item {
                DecompiledCodeItem::PreDeclareStatement { variable } => {
                    source.add_line(format!(
                        "let mut {};",
                        naming.variable(*variable),
                    ));
                }

                DecompiledCodeItem::PossibleAssignStatement {
                    variable,
                    value,
                    is_decl,
                    ..
                } => {
                    if *is_decl {
                        to_decl_source(
                            &mut source,
                            format!("// possible: let {} = ", naming.variable(*variable)).as_str(),
                            ";",
                            value,
                            naming,
                            true,
                        )?;
                    } else {
                        source.add_line(format!(
                            "// possible: {} = {};",
                            naming.variable(*variable),
                            value.to_source(naming, true)?
                        ));
                    }
                }

                DecompiledCodeItem::ReturnStatement(expr) => {
                    if root_block && can_obmit_return {
                        if !expr.is_empty_tuple() {
                            to_decl_source(&mut source, "", "", expr, naming, true)?;
                        }
                    } else {
                        if expr.is_empty_tuple() {
                            source.add_line(format!("return"));
                        } else {
                            to_decl_source(&mut source, "return ", "", expr, naming, true)?;
                        }
                    }
                }

                DecompiledCodeItem::AbortStatement(expr) => {
                    to_decl_source(
                        &mut source,
                        "abort ",
                        if iter.peek().is_none() { "" } else { ";" },
                        expr,
                        naming,
                        true,
                    )?;
                }

                DecompiledCodeItem::BreakStatement => {
                    if iter.peek().is_none() {
                        source.add_line(format!("break"));
                    } else {
                        source.add_line(format!("break;"));
                    }
                }

                DecompiledCodeItem::ContinueStatement => {
                    if iter.peek().is_none() {
                        source.add_line(format!("continue"));
                    } else {
                        source.add_line(format!("continue;"));
                    }
                }

                DecompiledCodeItem::CommentStatement(comment) => {
                    source.add_line(format!("/* {} */", comment));
                }

                DecompiledCodeItem::AssignStatement {
                    variable,
                    value,
                    is_decl,
                } => {
                    if *is_decl {
                        to_decl_source(
                            &mut source,
                            format!(
                                "let {}{} = ",
                                if mutable_variables.contains(variable) {
                                    "mut "
                                } else {
                                    ""
                                },
                                naming.variable(*variable)
                            )
                            .as_str(),
                            ";",
                            value,
                            naming,
                            true,
                        )?;
                    } else {
                        source.add_line(format!(
                            "{} = {};",
                            naming.variable(*variable),
                            value.to_source(naming, true)?
                        ));
                    }
                }

                DecompiledCodeItem::AssignTupleStatement {
                    variables,
                    value,
                    is_decl,
                } => {
                    source.add_line(format!(
                        "{}({}) = {};",
                        if *is_decl { "let " } else { "" },
                        variables
                            .iter()
                            .map(|v| format!(
                                "{}{}",
                                if *is_decl && mutable_variables.contains(v) {
                                    "mut "
                                } else {
                                    ""
                                },
                                naming.variable(*v)
                            ))
                            .collect::<Vec<_>>()
                            .join(", "),
                        value.to_source(naming, true)?
                    ));
                }

                DecompiledCodeItem::AssignStructureStatement {
                    structure_visible_name,
                    fields_count: _,
                    variables,
                    value,
                    ref_type,
                } => {
                    if variables.len() >= 2 {
                        source.add_line(format!("let {} {{", structure_visible_name));
                        let mut inner_unit = SourceCodeUnit::new(1);
                        let k_max_width = variables.iter().map(|(k, _)| k.len()).max().unwrap_or(0);

                        for (k, v) in variables {
                            inner_unit.add_line(format!(
                                "{:width$} : {}{},",
                                k,
                                match (mutable_variables.contains(v), ref_type) {
                                    (true, None)
                                    | (true, Some(move_model::model::RefType::ByValue)) => "mut ",
                                    (true, Some(move_model::model::RefType::ByImmRef)) =>
                                        unreachable!(),
                                    (true, Some(move_model::model::RefType::ByMutRef)) => "&mut ",
                                    (false, None)
                                    | (false, Some(move_model::model::RefType::ByValue)) => "",
                                    (false, Some(move_model::model::RefType::ByImmRef)) => "&",
                                    (false, Some(move_model::model::RefType::ByMutRef)) =>
                                        unreachable!(),
                                },
                                naming.variable(*v),
                                width = k_max_width
                            ));
                        }

                        source.add_block(inner_unit);
                        source.add_line(format!("}} = {};", value.to_source(naming, true)?));
                    } else {
                        source.add_line(format!(
                            "let {} {{ {} }} = {};",
                            structure_visible_name,
                            variables
                                .iter()
                                .map(|(k, v)| format!(
                                    "{}: {}{}",
                                    k,
                                    if mutable_variables.contains(v) {
                                        "mut "
                                    } else {
                                        ""
                                    },
                                    naming.variable(*v)
                                ))
                                .collect::<Vec<_>>()
                                .join(", "),
                            value.to_source(naming, true)?,
                        ));
                    }
                }

                DecompiledCodeItem::Statement { expr } => {
                    source.add_line(format!("{};", expr.to_source(naming, true)?));
                }

                DecompiledCodeItem::IfElseStatement {
                    cond,
                    if_unit,
                    else_unit,
                    result_variables,
                    use_as_result,
                } => {
                    let mut in_termination = InTerminationType::None;

                    let prefix = match use_as_result {
                        ResultUsageType::None => {
                            let_assignment_or_empty(result_variables, naming, mutable_variables)
                        }
                        ResultUsageType::Return => {
                            in_termination = InTerminationType::Return;
                            if can_obmit_return {
                                "".to_string()
                            } else {
                                "return ".to_string()
                            }
                        }
                        ResultUsageType::Abort => {
                            in_termination = InTerminationType::Abort;
                            "abort ".to_string()
                        }
                        ResultUsageType::BlockResult => "".to_string(),
                    };

                    source.add_line(format!(
                        "{}if ({}) {{",
                        prefix,
                        cond.to_source(naming, false)?,
                    ));

                    let mut if_b = if_unit.to_source(naming, false, mutable_variables)?;
                    if_b.add_indent(1);
                    source.add_block(if_b);

                    let else_b = to_source_maybe_else_chain(
                        else_unit,
                        naming,
                        &in_termination,
                        mutable_variables,
                    )?;

                    if !else_b.is_empty() {
                        source.add_block(else_b);
                    }

                    if use_as_result != &ResultUsageType::None {
                        source.add_line(format!("}}"));
                    } else {
                        source.add_line(format!("}};"));
                    }
                }

                DecompiledCodeItem::WhileStatement { cond, body } => {
                    if cond.is_none() {
                        source.add_line(format!("loop {{"));
                    } else {
                        source.add_line(format!(
                            "while ({}) {{",
                            cond.as_ref().unwrap().to_source(naming, false)?
                        ));
                    }

                    let mut b = body.to_source(naming, false, mutable_variables)?;
                    b.add_indent(1);
                    source.add_block(b);
                    source.add_line(format!("}};"));
                }

                DecompiledCodeItem::VariantSwitchStatement {
                    expr,
                    cases_ref_type: _,
                    local_variable: _,
                    cases,
                    result_variables,
                    use_as_result,
                } => {
                    let prefix = match use_as_result {
                        ResultUsageType::None => {
                            let_assignment_or_empty(result_variables, naming, mutable_variables)
                        }
                        ResultUsageType::Return => {
                            if can_obmit_return {
                                "".to_string()
                            } else {
                                "return ".to_string()
                            }
                        }
                        ResultUsageType::Abort => "abort ".to_string(),
                        ResultUsageType::BlockResult => "".to_string(),
                    };

                    source.add_line(format!(
                        "{}match ({}) {{",
                        prefix,
                        expr.to_source(naming, false)?,
                    ));

                    for case in cases {
                        let mut b = case.to_source(naming, false, mutable_variables)?;
                        b.add_indent(1);
                        source.add_block(b);
                    }

                    if use_as_result != &ResultUsageType::None {
                        source.add_line(format!("}}"));
                    } else {
                        source.add_line(format!("}};"));
                    }
                }
            }
        }

        if first {
            if let Some(v) = self.for_case_variable.as_ref() {
                wrapper_source.add_line(format!("{} => {{", v.default_match_case_expr));
                source.add_indent(1);
                finalize_wrapper = true;
            }
        }

        if let Some(value) = &self.exit {
            source.add_line(format!("{}", value.to_source(naming, true)?));
        }

        if finalize_wrapper {
            wrapper_source.add_block(source);
            // currently this is used only for match statement
            wrapper_source.add_line(format!("}},"));
            return Ok(wrapper_source);
        }

        Ok(source)
    }
}

fn should_follow_chain(else_unit: &DecompiledCodeUnit, in_termination: &InTerminationType) -> bool {
    if else_unit.exit.is_some() {
        return false;
    }
    if else_unit.blocks.len() != 1 {
        return false;
    }

    let DecompiledCodeItem::IfElseStatement {
        use_as_result,
        result_variables,
        ..
    } = &else_unit.blocks[0]
    else {
        return false;
    };

    if result_variables.len() > 0 {
        return false;
    }

    match (&in_termination, use_as_result) {
        (&InTerminationType::None, &ResultUsageType::Abort | &ResultUsageType::Return) => {
            return false;
        }
        (&InTerminationType::Return, &ResultUsageType::Abort) => {
            return false;
        }
        (&InTerminationType::Abort, &ResultUsageType::Return) => {
            return false;
        }
        _ => {}
    }

    true
}

fn to_source_maybe_else_chain(
    else_unit: &DecompiledCodeUnit,
    naming: &Naming<'_>,
    in_termination: &InTerminationType,
    mutable_variables: &HashSet<usize>,
) -> Result<SourceCodeUnit, anyhow::Error> {
    let mut unit = SourceCodeUnit::new(0);

    if !should_follow_chain(else_unit, &in_termination) {
        let mut else_b = else_unit
            .to_source(naming, false, mutable_variables)
            .unwrap();
        if !else_b.is_empty() {
            else_b.add_indent(1);
            unit.add_line("} else {".to_string());
            unit.add_block(else_b);
        }
    } else {
        let DecompiledCodeItem::IfElseStatement {
            cond,
            if_unit,
            else_unit: next_else_unit,
            ..
        } = &else_unit.blocks[0]
        else {
            unreachable!()
        };

        unit.add_line(format!(
            "}} else if ({}) {{",
            cond.to_source(naming, false)?,
        ));

        let mut if_b = if_unit.to_source(naming, false, mutable_variables)?;
        if_b.add_indent(1);
        unit.add_block(if_b);

        let else_b =
            to_source_maybe_else_chain(&next_else_unit, naming, in_termination, mutable_variables)?;

        if !else_b.is_empty() {
            unit.add_block(else_b);
        }
    }

    Ok(unit)
}

fn to_decl_source(
    source: &mut SourceCodeUnit,
    prefix: &str,
    suffix: &str,
    value: &DecompiledExpr,
    naming: &Naming<'_>,
    standalone: bool,
) -> Result<(), anyhow::Error> {
    let value = value.to_source_decl(naming, standalone)?;
    let value = prefix.to_string() + &value + suffix;
    let lines = value.split("\n").collect::<Vec<_>>();

    if lines.len() > 1 {
        source.add_line(lines[0].to_string());

        let mut inner_unit = SourceCodeUnit::new(1);

        for line in &lines[1..lines.len() - 1] {
            inner_unit.add_line(line.to_string());
        }

        source.add_block(inner_unit);
        source.add_line(lines[lines.len() - 1].to_string());
    } else {
        source.add_line(value);
    }

    Ok(())
}

fn let_assignment_or_empty(
    result_variables: &Vec<usize>,
    naming: &Naming,
    mutable_variables: &HashSet<usize>,
) -> String {
    if result_variables.is_empty() {
        String::new()
    } else {
        let vars = format!(
            "{}",
            result_variables
                .iter()
                .map(|v| format!(
                    "{}{}",
                    if mutable_variables.contains(v) {
                        "mut "
                    } else {
                        ""
                    },
                    naming.variable(*v)
                ))
                .collect::<Vec<_>>()
                .join(", ")
        );

        if result_variables.len() > 1 {
            format!("let ({}) = ", vars)
        } else {
            format!("let {} = ", vars)
        }
    }
}
