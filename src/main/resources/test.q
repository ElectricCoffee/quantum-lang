module Test;
object Program {
  define Unit #main = {
    val Num a = 3;
    val b = "hello world";

    if a > b then "hia";

    for i in Bar do "hello";

    for {
     i in K;
     j in F;
    } yield {foo;};

    match (foo) (Num bar) then stuff;
    match (bar) {
      "stuff" then stuff;
      (Num baz) then more-stuff;
    };
  };
  define string #foo(bar) = "hello";
  define num (string quux) = quux;
}