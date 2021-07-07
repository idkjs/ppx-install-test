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
    let%bind_open x = a;
    return(x + 1);
  };

  let _mf' = (a, b, c): X.t(_) => {
    let%bind_open x = a
    and y = b
    and (u, v) = c;
    return(x + y + u * v);
  };

  let _mg = (a): X.t(_) => {
    let%map x: X.t(int) = (a: X.t(int));
    x + 1;
  };

  let _mg' = (a, b, c): X.t(_) => {
    let%map x = a
    and y = b
    and (u, v) = c;
    x + y + u * v;
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
    open Let_syntax;
    let%mapn a = return(1)
    and b = return("hi")
    and c = return(2.34)
    and d = return(true);
    Printf.sprintf("%d %s %f %b", a, b, c, d);
  };
};
