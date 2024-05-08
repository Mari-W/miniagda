use super::lex::{Braced, SpannedToks, Token};
use crate::diagnostics::span::{Span, Spanned};
use crate::syntax::surface::{self, Cls, ClsAbsurd, ClsClause, Cstr, Ctx, Decl, Func, Pat, PatDot, PatId, Prog, Tm, TmAbs, TmAll, TmApp, TmSet};
use crate::syntax::Ident;

peg::parser! {
  pub grammar parser<'a>(file: &str) for SpannedToks<'a, Braced<Token<'a>>> {
    use Token::{Arrow, BraceL, BraceR, Colon, Data, Equals, Id, Lambda, ParenL, ParenR, Where, Dot, All};
    use Braced::{Begin, End, Item};
    use Braced::Token as Tok;

    rule id() -> Ident
        = start:position!() [Tok(Id(x))] end:position!() {
          Ident { name: x.to_owned(), span: Span { file: file.to_string(), start, end }
        }
    }

    // -----------------------------------------------------------------------------------------------------------------------------------
    // Terms

    rule tm() -> Tm
      = start:position!() ctx:ctx1() [Tok(Arrow)] codom:tm() end:position!() {
        ctx_to_all(&ctx.binds, codom, (file, (start, end)))
      }
      / tm:tm_no_fn() {tm}

    rule tm_no_fn() -> Tm = precedence!{
      [Tok(ParenL)] tm:tm() [Tok(ParenR)] { Tm::Brc(Box::new(tm)) }
      [Tok(BraceL)] _:id() [Tok(Equals)] tm:tm() [Tok(BraceR)] { Tm::Brc(Box::new(tm)) }
      --
      start:position!() [Tok(Lambda)] bind:bind() [Tok(Arrow)] body:tm() end:position!() {
          Tm::Abs(TmAbs { ident: bind.0, ty: Box::new(bind.1), body: Box::new(body), span: Span { file: file.to_string(), start, end }  })
      }
      --
      left:(@) right:tm_no_fn() end:position!() {
        let left_span = left.span();
        let right_span = right.span();
        roll_app(left, right)
      }
      ident:id() {
        // TODO: add respective token
        if ident.name.starts_with("Set") {
          if let Ok(level) = ident.name[3..].parse::<usize>() {
            return Tm::Set(TmSet { level, span: ident.span })
          }
          if ident.name.len() == 3 {
            return Tm::Set(TmSet { level: 0, span: ident.span })
          }
        }
        Tm::Var(ident)
      }
    }

    // -----------------------------------------------------------------------------------------------------------------------------------
    // Contexts

    rule bind() -> (Ident, Tm)
      = [Tok(ParenL)] ident:id() [Tok(Colon)] tm:tm() [Tok(ParenR)] { (ident, tm) }
      / [Tok(Arrow)]? pos:position!() tm:tm_no_fn() &[Tok(Arrow)] {
        (Ident { span: Span { file: file.to_string(), start: pos, end: pos }, name: "_".to_owned() }, tm)
      }

    rule ctx() -> Ctx
      = start:position!() binds:bind()* end:position!() {
        Ctx { binds, span: Span { file: file.to_string(), start, end } }
        }

    rule ctx1() -> Ctx
      = start:position!() [Tok(All)]? binds:bind()+ end:position!() {
        Ctx { binds, span: Span { file: file.to_string(), start, end } }
      }

    // -----------------------------------------------------------------------------------------------------------------------------------
    // Functions


    rule pat() -> Pat
      = start:position!() ident:id() end:position!() {
          Pat::Id(PatId { ident, pats: vec![], span: Span { file: file.to_string(), start, end } })
      }
      / start:position!() [Tok(ParenL)] ident:id() pats:pat()+ [Tok(ParenR)] end:position!() { Pat::Id(PatId { ident, pats, span: Span { file: file.to_string(), start, end } }) }
      / start:position!() [Tok(Dot)] [Tok(ParenL)] tm:tm() [Tok(ParenR)] end:position!() {
        Pat::Dot(PatDot { tm, span: Span { file: file.to_string(), start, end } })
      }
      // ident:id() { Pat::Var(ident) } // might be Pat::Cst -- check in surface_to_core

    rule cls() -> Cls
      = [Item] start:position!() ident:id() pats:pat()* [Tok(Equals)] [Begin] [Item] tm:tm() [End] end:position!() {
        Cls::Cls(ClsClause { func: ident, pats, rhs: tm, span: Span { file: file.to_string(), start, end } })
      }
      / [Item] start:position!() ident:id() pats:pat()* [Tok(BraceL)] [Tok(BraceR)] end:position!() {
        Cls::Abs(ClsAbsurd { func: ident, pats, span: Span { file: file.to_string(), start, end } })
      }

    rule func() -> Func
        = [Item] start:position!() ident:id() [Tok(Colon)] ty:tm() cls:cls()+ end:position!() {
           Func { ident, ty, cls, span: Span { file: file.to_string(), start, end } }
        }

    // -----------------------------------------------------------------------------------------------------------------------------------
    // Data Types


    rule cstr_rhs() -> (Ctx, Vec<Tm>)
      = ctx:ctx1() [Tok(Arrow)] tm:tm_no_fn() {
        (ctx, unroll_app(tm))
      }
      / pos:position!() tm:tm_no_fn() {
        (Ctx { binds: vec![], span: Span{ file: file.to_string(), start: pos, end: pos } }, unroll_app(tm))
      }

    rule cstr(data : &Ident) -> Cstr
      = start:position!() [Item] ident:id() [Tok(Colon)] ty:tm() end:position!() {
        Cstr { ident, ty, span: Span { file: file.to_string(), start, end } }
      }

    rule indices_and_level() -> (Ctx, Tm)
      = ctx:ctx1() [Tok(Arrow)] tm:tm_no_fn() {
        (ctx, tm)
      }
      / pos:position!() tm:tm_no_fn() {
        (Ctx { binds: vec![], span: Span{ file: file.to_string(), start:pos, end:pos } }, tm)
      }

    rule data() -> surface::Data
      = [Item]  start:position!() [Tok(Data)] ident:id() params:ctx() [Tok(Colon)] ial:indices_and_level() [Tok(Where)]
        [Begin] cstrs:cstr(&ident)* [Item]? [End] end:position!() {
          surface::Data { ident, params, indices: ial.0, set: ial.1, cstrs, span: Span { file: file.to_string(), start, end } }
      }


    // -----------------------------------------------------------------------------------------------------------------------------------
    // Programs

    rule decl() -> Decl
      = data:data() { Decl::Data(data) }
      / func:func() { Decl::Func(func) }

    pub rule prog() -> Prog
     = start:position!() [Begin] decls:decl()* [End] end:position!() {
      Prog { decls, span: Span { file: file.to_string(), start, end } }
     }
  }
}

fn ctx_to_all(binds: &[(Ident, Tm)], codom: Tm, (file, (start, end)): (&str, (usize, usize))) -> Tm {
  assert!(!binds.is_empty(), "ice: expected non empty context to perform a translation to type");

  let codom = if binds.len() == 1 {
    codom
  } else {
    ctx_to_all(&binds[1..], codom, (file, (binds[1..][0].0.span().start, end)))
  };

  Tm::All(TmAll {
    ident: binds[0].0.clone(),
    dom: Box::new(binds[0].1.clone()),
    codom: Box::new(codom),
    span: Span {
      file: file.to_string(),
      start,
      end,
    },
  })
}

fn unroll_app(app: Tm) -> Vec<Tm> {
  match app {
    Tm::App(TmApp { left, right, .. }) => {
      let mut v = unroll_app(*left);
      v.push(*right);
      v
    }
    tm => vec![tm],
  }
}

fn roll_app(left: Tm, right: Tm) -> Tm {
  let left_span = left.span();
  let right_span = right.span();
  match right {
    Tm::App(TmApp { left: left1, right: right1, .. }) => Tm::App(TmApp {
      left: Box::new(roll_app(left, *left1)),
      right: right1,
      span: Span {
        file: left_span.file.to_string(),
        start: left_span.start,
        end: right_span.end,
      },
    }),
    tm => Tm::App(TmApp {
      left: Box::new(left),
      right: Box::new(tm),
      span: Span {
        file: left_span.file.clone(),
        start: left_span.start,
        end: right_span.end,
      },
    }),
  }
}
