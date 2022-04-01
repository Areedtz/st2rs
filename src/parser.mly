%{
  open Types
%}

%token <string> ID
%token COMMA COLON SEMI PCT ARROW BIGARROW AT AUTH CONF AUTHCONF
%token LEFT_PAR RIGHT_PAR LEFT_ANGLE RIGHT_ANGLE LEFT_BRACE RIGHT_BRACE LEFT_BRACK RIGHT_BRACK
%token EQ AND OR NOT
%token NEW LET EVENT INJ_EVENT IN END IF ELSE
%token PROBLEM PRINCIPALS KNOWLEDGE TYPES FUNCTIONS EQUATIONS FORMATS EVENTS QUERIES PROTOCOL DISHONEST
%token EOF

%start <Types.problem option> program
%%

opt_knowledge:
| KNOWLEDGE; COLON; k = separated_list(COMMA, indef); SEMI; { k }
| { [] }

opt_formats:
| FORMATS; COLON; formats = separated_list(COMMA, format_def); SEMI; { formats }
| { [] }

opt_events:
| EVENTS; COLON; e = separated_list(COMMA, evdef); SEMI; { e }
| { [] }

opt_queries:
| QUERIES; COLON; q = separated_list(COMMA, qdef); SEMI; { q }
| { [] }

program:
| PROBLEM; COLON; n = ID; SEMI;
  PRINCIPALS; COLON; p = separated_list(COMMA, prindef); SEMI;
  k = opt_knowledge;
  TYPES; COLON; t = separated_list(COMMA, data_type); SEMI;
  FUNCTIONS; COLON; f = separated_list(COMMA, fundef); SEMI;
  EQUATIONS; COLON; e = separated_list(COMMA, eqdef); SEMI;
  formats = opt_formats;
  events = opt_events;
  queries = opt_queries;
  PROTOCOL; COLON; g = global_type; EOF
{ Some { name = n; principals = p; knowledge = k; types = t; functions = f; equations = e; formats = formats; events = events; queries = queries; protocol = g } };

fundef:
| f = ID; LEFT_PAR; params = data_type_list; RIGHT_PAR; ARROW; return_type = data_type { (f, (params, return_type, false, [])) }
| f = ID; LEFT_ANGLE; dtype = data_type; RIGHT_ANGLE; LEFT_PAR; params = data_type_list; RIGHT_PAR; ARROW; return_type = data_type { (f, (params, return_type, false, [dtype])) }

eqdef:
| lhs = term; EQ; rhs = term { (lhs, rhs) }

indef:
| t = ID; COLON; dt = data_type; AT; prin = ID { (t, dt, prin, Null) }
| t = ID; COLON; dt = data_type; AT; prin = ID; EQ; f = term { (t, dt, prin, f) }

evdef:
| e = ID; LEFT_PAR; params = data_type_list; RIGHT_PAR { (e, params) }

qdef:
| event = event { ReachQuery(event) }
| event = event; BIGARROW; q = qdef { CorrQuery(event, q) }

event:
| INJ_EVENT; LEFT_PAR; e = ID; LEFT_PAR; args = term_list; RIGHT_PAR; RIGHT_PAR;
  { InjEvent(e, args) }
| EVENT; LEFT_PAR; e = ID; LEFT_PAR; args = term_list; RIGHT_PAR; RIGHT_PAR;
  { NonInjEvent(e, args) }

prindef:
| name = ID; LEFT_BRACK; DISHONEST; RIGHT_BRACK
    { name, true }
| name = ID
    { name, false }

format_def:
| f = ID; LEFT_PAR; params = data_type_list; RIGHT_PAR { (f, params) }


(* Choose? *)
term:
| name = ID
  { Var(name) }
| name = ID; LEFT_PAR; args = term_list; RIGHT_PAR
  { Func(name, args) }
| name = ID; LEFT_ANGLE; args = term_list; RIGHT_ANGLE
  { Form(name, args) }
| LEFT_ANGLE; args = term_list; RIGHT_ANGLE
  { Tuple(args) }
| t1 = term; EQ; t2 = term
  { Eq(t1, t2) }
| t1 = term; AND; t2 = term
  { And(t1, t2) }
| t1 = term; OR; t2 = term
  { Or(t1, t2) }
| NOT; t = term
  { Not(t) }
| LEFT_PAR; t = term; RIGHT_PAR
  { t }
| IF; LEFT_PAR; cond = term; COMMA; tterm = term; COMMA; fterm = term; RIGHT_PAR
  { IfAssign(cond, tterm, fterm) };

term_list:
| l = separated_list(COMMA, term)
  { l };

data_type:
| name = ID { DType(name) }
| wrapper = ID; LEFT_ANGLE; name = ID; RIGHT_ANGLE { DAType(wrapper, name) }
| LEFT_ANGLE; l = data_type_list; RIGHT_ANGLE { DTType(l) }

data_type_list:
  | l = separated_list(COMMA, data_type)
    { l };

pattern:
| LEFT_PAR; p = pattern; RIGHT_PAR 
  { p }
| name = ID
  { PVar(name, None) }
| name = ID; COLON; dt = data_type
  { PVar(name, dt) }
| PCT; t = term
  { PMatch(t) }
| name = ID; LEFT_ANGLE; pargs = pattern_list; RIGHT_ANGLE
  { PForm(name, pargs) }
| LEFT_ANGLE; pargs = pattern_list; RIGHT_ANGLE
  { PTuple(pargs) }

pattern_list:
| l = separated_list(COMMA, pattern)
  { l };

let_bind:
| NEW; name = ID; COLON; data = data_type SEMI; letb = let_bind
  { New(name, data, letb) }
| LET; p = pattern; EQ; t = term; SEMI; letb = let_bind
  { Let(p, t, letb) }
| EVENT; name = ID; LEFT_PAR; ts = term_list; RIGHT_PAR; SEMI; letb = let_bind
  { Event(name, ts, letb) }
| IF; LEFT_PAR; cond = term; RIGHT_PAR; LEFT_BRACE; then_body = let_bind; RIGHT_BRACE; ELSE; LEFT_BRACE; else_body = let_bind; RIGHT_BRACE
  { IfBlock(cond, then_body, else_body) }
| { LetEnd };

channel_option:
| ARROW { Public }
| AUTH { Auth  }
| CONF { Conf }
| AUTHCONF { AuthConf };

global_type:
| prin1 = ID; chan = channel_option; prin2 = ID; COLON; x = ID; EQ; t = term; gt = global_type
  { Send(prin1, prin2, chan, x, t, gt ) }
| prin1 = ID; chan = channel_option; prin2 = ID; LEFT_BRACE; ID; COLON; lb = global_type; ID; COLON; rb = global_type; RIGHT_BRACE;
  { Branch(prin1, prin2, chan, lb, rb) }
| prin = ID; LEFT_BRACE; lb = let_bind; RIGHT_BRACE; gt = global_type
  { Compute(prin, lb, gt) }
| LET; name = ID; EQ; LEFT_BRACE; gt1 = global_type; RIGHT_BRACE; IN; gt2 = global_type
  { DefGlobal(name, gt1, gt2) }
| name = ID; LEFT_PAR; RIGHT_PAR
  { CallGlobal(name) }
| END
  { GlobalEnd };

param:
| x = ID; COLON; dt = data_type; AT; p = ID { ((x, dt), p) }