module Test;
receiver Program {
  define Unit #main = {
    val Num a = 3;
    val b = "hello world";

    if a > b then "hia";

    for i in Bar do "hello";

    for {
     i in K;
     j in F;
    } yield i + j;

    match ("foo") (Num bar) then "Hola!";
    match ("bar") {
      "stuff" then "ding";
      (Num baz) then "mehr ding";
    };
  };
  define string #foo(bar) = "hello";
  define num (string quux) = quux;
}