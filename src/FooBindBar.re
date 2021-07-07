Js.log("Hello, ReScript!");

module Monad_example = {
  module X: {
    type t('a);
    module Let_syntax: {
      let return: 'a => t('a);
      module Let_syntax: {
        let return: 'a => t('a);
        let bind: (t('a), ~f: 'a => t('b)) => t('b);
        let map: (t('a), ~f: 'a => 'b) => t('b);
        let both: (t('a), t('b)) => t(('a, 'b));
        module Open_on_rhs: {let return: 'a => t('a);};
      };
    };
  } = {
    type t('a) = 'a;
    let return = x => x;
    let bind = (x, ~f) => f(x);
    let map = (x, ~f) => f(x);
    let both = (x, y) => (x, y);
    module Let_syntax = {
      let return = return;
      module Let_syntax = {
        let return = return;
        let bind = bind;
        let map = map;
        let both = both;
        module Open_on_rhs = {
          let return = return;
        };
      };
    };
  };
  open X.Let_syntax;
  let _mf = (a): X.t(_) => {
    %bind_open
    {
      let x = a;
      return(x + 1);
    };
  };
  let _mf' = (a, b, c): X.t(_) => {
    %bind_open
    {
      let x = a
      and y = b
      and (u, v) = c;
      return(x + y + u * v);
    };
  };
  let _mg = (a): X.t(_) => {
    %map
    {
      let x: X.t(int) = a;
      x + 1;
    };
  };
  let _mg' = (a, b, c): X.t(_) => {
    %map
    {
      let x = a
      and y = b
      and (u, v) = c;
      x + y + u * v;
    };
  };
  let _mh = (a): X.t(_) =>
    switch%bind_open (a) {
    | 0 => return(true)
    | _ => return(false)
    };
  let _mi = (a): X.t(_) =>
    switch%map (a) {
    | 0 => true
    | _ => false
    };
  let _mif = (a): X.t(_) =>
    if%bind_open (a) {
      return(true);
    } else {
      return(false);
    };
  let _mif' = (a): X.t(_) => if%map (a) {true} else {false};
};

module Applicative_example = {
  module X: {
    type t('a);
    module Let_syntax: {
      let return: 'a => t('a);
      module Let_syntax: {
        let return: 'a => t('a);
        let map: (t('a), ~f: 'a => 'b) => t('b);
        let both: (t('a), t('b)) => t(('a, 'b));
        module Open_on_rhs: {
          let flag: t(int);
          let anon: t(int);
        };
      };
    };
  } = {
    type t('a) = 'a;
    let return = x => x;
    let map = (x, ~f) => f(x);
    let both = (x, y) => (x, y);
    module Let_syntax = {
      let return = return;
      module Let_syntax = {
        let return = return;
        let map = map;
        let both = both;
        module Open_on_rhs = {
          let flag = 66;
          let anon = 77;
        };
      };
    };
  };
  open X.Let_syntax;
  /* {[
       let _af a : _ X.t =
         let%bind x = a in (* "Error: Unbound value Let_syntax.bind" *)
         return (x + 1)
     ]} */
  /* {[
       let _af' a b c : _ X.t =
         let%bind x = a and y = b and (u, v) = c in (* "Error: Unbound value Let_syntax.bind" *)
         return (x + y + (u * v))
     ]} */
  let _ag = (a): X.t(_) => {
    %map
    {
      let x = a;
      x + 1;
    };
  };
  let _ag' = (a, b, c): X.t(_) => {
    %map
    {
      let x = a
      and y = b
      and (u, v) = c;
      x + y + u * v;
    };
  };
  /* {[
       let _ah a : _ X.t =
         match%bind a with (* "Error: Unbound value Let_syntax.bind" *)
         | 0 -> return true
         | _ -> return false
     ]} */
  let _ai = (a): X.t(_) =>
    switch%map (a) {
    | 0 => true
    | _ => false
    };
};

module Example_without_open = {
  let _ag = (a): Applicative_example.X.t(_) => {
    %map.Applicative_example.X
    {
      let x = a;
      x + 1;
    };
  };
};

module Example_with_mapn = {
  module Let_syntax = {
    let return = Monad_example.X.Let_syntax.return;
    module Let_syntax = {
      include Monad_example.X.Let_syntax.Let_syntax;
      let map2 = (a, b, ~f) => map(both(a, b), ~f=((a, b)) => f(a, b));
      let map3 = (a, b, c, ~f) =>
        map2(both(a, b), c, ~f=((a, b), c) => f(a, b, c));
      let map4 = (a, b, c, d, ~f) =>
        map2(both(a, b), both(c, d), ~f=((a, b), (c, d)) =>
          f(a, b, c, d)
        );
    };
  };
  let _x = {
    open! Let_syntax;
    %mapn
    {
      let a = return(1)
      and b = return("hi")
      and c = return(2.34)
      and d = return(true);
      // Printf.sprintf("%d %s %f %b", a, b, c, d);
      Js.log4(a, b, c, d);
    };
  };
};
Js.log("Example_with_mapn!");
Js.log(Example_with_mapn._x);
let xta: Applicative_example.X.t('a) =
  Applicative_example._ag(Applicative_example.X.Let_syntax.return(1));

Js.log("Example_without_open!");
Js.log(Example_without_open._ag(xta));
