const PREC = {
  postfix: 1,
  assign: 1,
  infix: 2,
  new: 3,
  prefix: 3,
  compound: 3,
  // func: 3,
  call: 4,
  field: 4,
}

module.exports = grammar({
  name: 'scala',

  extras: $ => [
    /\s/,
    $.comment
  ],

  supertypes: $ => [
    $._expression,
    $._definition,
    $._pattern,
  ],

  externals: $ => [
    $._automatic_semicolon,
    $._simple_string,
    $._string_start,
    $._string_middle,
    $._string_end,
    $._multiline_string_start,
    $._multiline_string_middle,
    $._multiline_string_end,
    'else',
  ],

  inline: $ => [
    $._pattern,
    $._semicolon,
    $._definition,
    $._definition_in_block,
    $._type_identifier,
    $._param_type,
    $._block_expression,
    $._arguments,
    $._expression,
    $._binding,
    $._import_expression,

    $._block,
    $._simple_type,
    $._pattern2,
    $._type,
  ],

  conflicts: $ => [
    [$.tuple_type, $.parameter_types],
    [$.tuple_expression, $.function_parameters],
    [$.parenthesized_expression, $.function_parameters],
    [$.parameter_types, $.function_parameters], // identifier  ':'  _annotated_type  •  '=>'

    [$.import_declaration, $.stable_identifier],
    [$.val_declaration, $.val_definition],
    [$.var_declaration, $.var_definition],
    [$.stable_type_identifier, $.stable_identifier], // 'extends'  identifier  '.'  identifier  •  '.'

    // [$._expression, $._binding],
    // [$.call_expression, $.call_expression],
    // [$.parenthesized_expression, $.function_parameters],
    // [$.class_definition, $.class_definition],
    // [$._expression, $.function_expression],
  ],

  word: $ => $.identifier,

  rules: {
    compilation_unit: $ => repeat($._definition),

    _definition: $ => choice(
      $.package_clause,
      $.package_object,
      $._definition_in_block,
    ),

    _definition_in_block: $ => choice(
      $.class_definition,
      $.import_declaration,
      $.object_definition,
      $.trait_definition,
      $.val_definition,
      $.val_declaration,
      $.var_definition,
      $.var_declaration,
      $.type_definition,
      $.function_definition,
      $.function_declaration,
    ),

    package_clause: $ => seq(
      'package',
      field('name', $.package_identifier),
      // This is slightly more permissive than the EBNF in that it allows any
      // kind of delcaration inside of the package blocks. As we're more
      // concerned with the structure rather than the validity of the program
      // we'll allow it.
      field('body', optional($.template_body))
    ),

    package_identifier: $ => sep1(
      '.', $.identifier
    ),

    package_object: $ => seq(
      'package',
      'object',
      $._object_definition
    ),

    import_declaration: $ => prec.right(seq(
      'import',
      sep1(',', $._import_expression)
    )),

    _import_expression: $ => seq(
      field('path', choice($.stable_identifier, $.identifier)),
      optional(seq(
        '.',
        choice(
          $.wildcard,
          $.import_selectors
        )
      ))
    ),

    import_selectors: $ => seq(
      '{',
      commaSep1(choice(
        $.identifier,
        $.renamed_identifier
      )),
      '}'
    ),

    renamed_identifier: $ => seq(
      field('name', $.identifier),
      '=>',
      field('alias', choice($.identifier, $.wildcard))
    ),

    object_definition: $ => seq(
      optional($.modifiers),
      optional('case'),
      'object',
      $._object_definition
    ),

    _object_definition: $ => prec.right(seq(
      field('name', $.identifier),
      field('extend', optional($.extends_clause)),
      field('body', optional($.template_body)),
    )),

    class_definition: $ => prec.right(seq(
      repeat($.annotation),
      optional($.modifiers),
      optional('case'),
      'class',
      field('name', $.identifier),
      field('type_parameters', optional($.type_parameters)),
      field('class_parameters', repeat($.class_parameters)),
      field('extend', optional($.extends_clause)),
      field('body', optional($.template_body))
    )),

    trait_definition: $ => prec.right(seq(
      optional($.modifiers),
      'trait',
      field('name', $.identifier),
      field('type_parameters', optional($.type_parameters)),
      field('extend', optional($.extends_clause)),
      field('body', optional($.template_body))
    )),

    // The EBNF makes a distinction between function type parameters and other
    // type parameters as you can't specify variance on function type
    // parameters. This isn't important to the structure of the AST so we don't
    // make that distinction.
    type_parameters: $ => seq(
      '[',
      commaSep1($._variant_type_parameter),
      ']'
    ),

    _variant_type_parameter: $ => seq(
      repeat($.annotation),
      choice(
        $.covariant_type_parameter,
        $.contravariant_type_parameter,
        $._type_parameter // invariant type parameter
      )
    ),

    covariant_type_parameter: $ => seq(
      '+',
      $._type_parameter
    ),

    contravariant_type_parameter: $ => seq(
      '-',
      $._type_parameter,
    ),

    _type_parameter: $ => seq(
      field('name', choice($.wildcard, $.identifier)),
      field('type_parameters', optional($.type_parameters)),
      field('bound', optional($.upper_bound)),
      field('bound', optional($.lower_bound)),
      field('bound', optional(repeat($.view_bound))),
      field('bound', optional(repeat($.context_bound))),
    ),

    upper_bound: $ => seq('<:', field('type', $._type)),

    lower_bound: $ => seq('>:', field('type', $._type)),

    view_bound: $ => seq('<%', field('type', $._type)),

    context_bound: $ => seq(':', field('type', $._type)),

    template_body: $ => seq(
      '{',
      // TODO: self type
      optional(prec.left($._block)),
      '}'
    ),

    annotation: $ => prec.right(seq(
      '@',
      field('name', $._simple_type),
      field('arguments', repeat($.arguments)),
    )),

    val_definition: $ => seq(
      repeat($.annotation),
      optional($.modifiers),
      'val',
      field('pattern', $._pattern2),
      optional(seq(':', field('type', $._type))),
      '=',
      field('value', $._expression)
    ),

    val_declaration: $ => seq(
      repeat($.annotation),
      optional($.modifiers),
      'val',
      commaSep1(field('name', $.identifier)),
      ':',
      field('type', $._type)
    ),

    var_declaration: $ => seq(
      repeat($.annotation),
      optional($.modifiers),
      'var',
      commaSep1(field('name', $.identifier)),
      ':',
      field('type', $._type)
    ),

    // TODO: ids ':' Type '=' '_'
    var_definition: $ => seq(
      repeat($.annotation),
      optional($.modifiers),
      'var',
      field('pattern', $._pattern2),
      optional(seq(':', field('type', $._type))),
      '=',
      field('value', $._expression)
    ),

    type_definition: $ => seq(
      repeat($.annotation),
      optional($.modifiers),
      'type',
      field('name', $._type_identifier),
      field('type_parameters', optional($.type_parameters)),
      '=',
      field('type', $._type)
    ),

    function_definition: $ => seq(
      repeat($.annotation),
      optional($.modifiers),
      'def',
      field('name', $.identifier),
      field('type_parameters', optional($.type_parameters)),
      field('parameters', repeat($.parameters)),
      optional(seq(':', field('return_type', $._type))),
      choice(
        seq('=', field('body', $._expression)),
        field('body', $.block)
      )
    ),

    function_declaration: $ => prec.right(seq(
      repeat($.annotation),
      optional($.modifiers),
      'def',
      field('name', $.identifier),
      field('type_parameters', optional($.type_parameters)),
      field('parameters', repeat($.parameters)),
      optional(seq(':', field('return_type', $._type)))
    )),

    modifiers: $ => repeat1(choice(
      'abstract',
      'final',
      'sealed',
      'implicit',
      'lazy',
      'override',
      'private',
      'protected'
    )),

    extends_clause: $ => prec.right(seq(
      'extends',
      field('type', $._type),
      optional($.arguments)
    )),

    // TODO: Allow only the last parameter list to be implicit.
    class_parameters: $ => seq(
      '(',
      optional('implicit'),
      commaSep($.class_parameter),
      ')'
    ),

    // TODO: Allow only the last parameter list to be implicit.
    parameters: $ => seq(
      '(',
      optional('implicit'),
      commaSep($.parameter),
      ')'
    ),

    // TODO: Support modifiers (conflict with implicit param list).
    class_parameter: $ => seq(
      repeat($.annotation),
      optional(choice('val', 'var')),
      field('name', $.identifier),
      ':',
      field('type', $._type),
      optional(seq('=', field('default_value', $._expression)))
    ),

    parameter: $ => seq(
      repeat($.annotation),
      field('name', $.identifier),
      optional(seq(':', field('type', $._param_type))),
      optional(seq('=', field('default_value', $._expression)))
    ),

    // Block
    _block: $ => seq(
      sep1($._semicolon, choice(
        $._expression,
        $._definition_in_block,
      )),
      optional($._semicolon),
    ),

    // '{' Block '}'
    block: $ => seq(
      '{',
      optional($._block),
      '}'
    ),

    // ---------------------------------------------------------------
    // Types

    _type: $ => choice(
      $.function_type,
      $.compound_type,
      $.infix_type,
      $._annotated_type,
    ),

    // TODO: Make this a visible type, so that _type can be a supertype.
    _annotated_type: $ => prec.right(seq(
      $._simple_type,
      repeat($.annotation),
    )),

    _simple_type: $ => choice(
      $.generic_type,
      $.projected_type,
      $.tuple_type,
      $.stable_type_identifier,
      $._type_identifier,
    ),

    compound_type: $ => prec(PREC.compound, seq(
      field('base', $._annotated_type),
      repeat1(seq('with', field('extra', $._annotated_type))),
      // TODO: Refinement.
    )),

    infix_type: $ => prec.left(PREC.infix, seq(
      field('left', choice($.compound_type, $.infix_type, $._annotated_type)),
      field('operator', choice($.identifier, $.operator_identifier)),
      field('right', choice($.compound_type, $.infix_type, $._annotated_type))
    )),

    tuple_type: $ => seq(
      '(',
      commaSep1($._type),
      ')',
    ),

    stable_type_identifier: $ => seq(
      choice($.identifier, $.stable_identifier),
      '.',
      $._type_identifier
    ),

    stable_identifier: $ => seq(
      choice($.identifier, $.stable_identifier),
      '.',
      $.identifier
    ),

    generic_type: $ => seq(
      field('type', $._simple_type),
      field('type_arguments', $.type_arguments)
    ),

    projected_type: $ => seq(
      field('type', $._simple_type),
      '#',
      field('selector', $._type_identifier),
    ),

    function_type: $ => prec.right(seq(
      field('parameter_types', $.parameter_types),
      '=>',
      field('return_type', $._type)
    )),

    // Deprioritize against typed_pattern._type and typed_pattern
    parameter_types: $ => prec(-2, choice(
      $._annotated_type,
      // Prioritize a parenthesized param list over a single tuple_type.
      prec.dynamic(1, seq('(', commaSep($._param_type), ')' )),
      $.compound_type,
      $.infix_type,
    )),

    _param_type: $ => choice(
      $._type,
      $.lazy_parameter_type,
      $.repeated_parameter_type,
    ),

    lazy_parameter_type: $ => seq(
      '=>',
      field('type', $._type)
    ),

    repeated_parameter_type: $ => seq(
      field('type', $._type),
      '*',
    ),

    _type_identifier: $ => alias($.identifier, $.type_identifier),

    // ---------------------------------------------------------------
    // Patterns

    _pattern: $ => choice(
      $.alternative_pattern,
      $.typed_pattern,
      $._pattern2,
    ),

    _pattern2: $ => choice(
      $.identifier,
      $.capture_pattern,
      $.tuple_pattern,
      $.case_class_pattern,
      $.infix_pattern,
      $.number,
      $.string,
      $.wildcard
    ),

    case_class_pattern: $ => seq(
      field('type', choice($._type_identifier, $.stable_type_identifier)),
      '(',
      field('pattern', commaSep($._pattern)),
      ')'
    ),

    infix_pattern: $ => prec.left(PREC.infix, seq(
      field('left', $._pattern),
      field('operator', choice($.identifier, $.operator_identifier)),
      field('right', $._pattern),
    )),

    capture_pattern: $ => prec(PREC.assign, seq(
      field('name', $.identifier),
      '@',
      field('pattern', $._pattern)
    )),

    typed_pattern: $ => prec(-1, seq(
      field('pattern', $._pattern),
      ':',
      field('type', $._type)
    )),

    // TODO: Flatten this.
    alternative_pattern: $ => prec.left(-2, seq(
      $._pattern,
      '|',
      $._pattern
    )),

    tuple_pattern: $ => seq(
      '(',
      $._pattern,
      repeat1(seq(',', $._pattern)),
      ')'
    ),

    // ---------------------------------------------------------------
    // Expressions

    _expression: $ => choice(
      $.if_expression,
      $.while_expression,
      $.do_while_expression,
      $.match_expression,
      $.try_expression,
      $.call_expression,
      $.generic_function,
      $.assignment_expression,
      $.parenthesized_expression,
      $.string_transform_expression,
      $.field_expression,
      $.instance_expression,
      // TODO: ascription
      $.postfix_expression,
      $.infix_expression,
      $.prefix_expression,
      $.tuple_expression,
      $._block_expression,
      $.function_expression,
      $.identifier,
      $.number,
      $.string
    ),

    _simple_expression: $ => choice(
      $.instance_expression,
      $._block_expression,
    ),

    postfix_expression: $ => prec(PREC.postfix, seq(
      field('operand', $._expression),
      field('operator', $.identifier),
    )),

    // BlockExpr
    _block_expression: $ => choice($.block, $.case_block),

    if_expression: $ => prec.right(seq(
      'if',
      field('condition', $.parenthesized_expression),
      field('consequence', $._expression),
      optional(seq(
        'else',
        field('alternative', $._expression)
      ))
    )),

    while_expression: $ => seq(
      'while',
      field('condition', $.parenthesized_expression),
      field('body', $._expression),
    ),

    do_while_expression: $ => seq(
      'do',
      field('body', $._expression),
      optional($._semicolon),
      'while',
      field('condition', $.parenthesized_expression),
    ),

    // TODO: Give this its own precedence?
    match_expression: $ => prec(PREC.postfix, seq(
      field('value', $._expression),
      'match',
      field('body', $.case_block)
    )),

    try_expression: $ => prec.right(seq(
      'try',
      field('body', $._expression),
      optional($.catch_clause),
      optional($.finally_clause)
    )),

    catch_clause: $ => prec.right(seq('catch', $.case_block)),

    finally_clause: $ => prec.right(seq('finally', $._expression)),

    case_block: $ => choice(
      prec(-1, seq('{', '}')),
      seq('{', repeat1($.case_clause), '}')
    ),

    case_clause: $ => prec.left(seq(
      'case',
      field('pattern', $._pattern),
      optional($.guard),
      '=>',
      field('body', optional(prec.left($._block))),
    )),

    guard: $ => seq(
      'if',
      field('condition', $._expression)
    ),

    assignment_expression: $ => prec.right(PREC.assign, seq(
      field('left', $._expression),
      '=',
      field('right', $._expression)
    )),

    // SimpleExpr TypeArgs
    generic_function: $ => prec(PREC.call, seq(
      field('function', $._expression),
      field('type_arguments', $.type_arguments)
    )),

    // SimpleExpr1 ArgumentExprs
    call_expression: $ => prec(PREC.call, seq(
      field('function', $._expression),
      $._arguments,
    )),

    // ArgumentExprs.
    _arguments: $ => choice(
      field('arguments', $.arguments),
      field('body', $._block_expression),
    ),

    field_expression: $ => prec(PREC.field, seq(
      field('value', $._expression),
      '.',
      field('field', $.identifier)
    )),

    instance_expression: $ => prec(PREC.new, seq(
      'new',
      $._expression
    )),

    infix_expression: $ => prec.left(PREC.infix, seq(
      field('left', $._expression),
      field('operator', choice($.identifier, $.operator_identifier)),
      field('right', $._expression)
    )),

    prefix_expression: $ => prec(PREC.prefix, seq(
      choice('+', '-', '!', '~'),
      $._expression
    )),

    tuple_expression: $ => seq(
      '(',
      $._expression,
      repeat1(seq(',', $._expression)),
      ')'
    ),

    parenthesized_expression: $ => seq(
      '(',
      $._expression,
      ')'
    ),

    type_arguments: $ => seq(
      '[',
      commaSep1($._type),
      ']'
    ),

    //  '(' [Exprs] ')'  TODO: ':_*'
    arguments: $ => seq(
      '(',
      commaSep($._expression),
      ')'
    ),

    // The EBNF seems to have a bug here. It wouldn't allow id => Block in a Block. This is actually
    // the combination of parts of Expr and ResultExpr that include Bindings.
    function_expression: $ => seq(
      field('function_parameters', $.function_parameters),
      '=>',
      // Normally $._block should have left associativity, but when the containing $.function_expression is
      // inside a $.block, we want $._block to be as long as possible. TODO: Figure out how to
      // determine whether we are in a $.block.
      field('body', prec.right($._block)),
    ),

    // Deprioritize against guard.
    function_parameters: $ => prec(-1, choice(
      // Prioritize a parenthesized param list over a single tuple_expression.
      prec.dynamic(1, seq('(', commaSep($._binding), ')')),
      $._binding,
    )),

    _binding: $ => seq(
      choice($.identifier, $.wildcard),
      optional(seq(':', $._type)),
    ),

    // TODO: Include operators.
    identifier: $ => /[a-zA-Z_]\w*/,

    wildcard: $ => '_',

    operator_identifier: $ => /[^\s\w\(\)\[\]\{\}'"`\.;,]+/,

    number: $ => /[\d\.]+/,

    string_transform_expression: $ => seq(
      $.identifier,
      $.string
    ),

    string: $ => choice(
      $._simple_string,
      seq(
        $._string_start,
        $.interpolation,
        repeat(seq(
          $._string_middle,
          $.interpolation,
        )),
        $._string_end
      ),
      seq(
        $._multiline_string_start,
        $.interpolation,
        repeat(seq(
          $._multiline_string_middle,
          $.interpolation,
        )),
        $._multiline_string_end
      )
    ),

    interpolation: $ => seq('$', choice($.identifier, $.block)),

    _semicolon: $ => choice(
      ';',
      $._automatic_semicolon
    ),

    comment: $ => token(choice(
      seq('//', /.*/),
      seq(
        '/*',
        /[^*]*\*+([^/*][^*]*\*+)*/,
        '/'
      )
    ))
  }
})

function commaSep(rule) {
  return optional(commaSep1(rule))
}

function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)))
}

function sep(delimiter, rule) {
  return optional(sep1(delimiter, rule))
}

function sep1(delimiter, rule) {
  return seq(rule, repeat(seq(delimiter, rule)))
}
