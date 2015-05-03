import scala.io._
import cs162.assign4.syntax._
import Aliases._
import scala.io.Source.fromFile

//——————————————————————————————————————————————————————————————————————————————
// Main entry point

object Checker {
  type TypeEnv = Map[Var, Type]
  val TypeEnv = Map
  object Illtyped extends Exception

  def main( args:Array[String] ) {
    val filename = args(0)
    val input = fromFile(filename).mkString

    Parsers.program.run(input, filename) match {
      case Left(e) => // parse error
        println(e)

      case Right(program) =>
        Checker(program.typedefs).getType(program.e, TypeEnv())
        println("This program is well-typed")
    }
  }
}

case class Checker(typeDefs: Set[TypeDef]) {
  import Checker.{ TypeEnv, Illtyped }

  // Gets a listing of the constructor names associated with a given
  // type definition. For example, consider the following type
  // definition:
  //
  // type Either['A, 'B] = Left 'A | Right 'B
  //
  // Some example calls to `constructors`, along with return values:
  //
  // constructors("Either") = Set("Left", "Right")
  // constructors("Foo") = a thrown Illtyped exception
  //
  def constructors(name: NameLabel): Set[ConsLabel] =
    typeDefs.
      find(_.name == name).
      map(_.constructors.keySet).
      getOrElse(throw Illtyped)

  // Takes the following parameters:
  // -The name of a user-defined type
  // -The name of a user-defined constructor in that user-defined type
  // -The types which we wish to apply to the constructor
  // Returns the type that is held within the constructor.
  //
  // For example, consider the following type definition:
  //
  // type Either['A, 'B] = Left 'A | Right 'B
  //
  // Some example calls to `constructorType`, along with return values:
  //
  // constructorType("Either", "Left", Seq(NumT, BoolT)) = NumT
  // constructorType("Either", "Right", Seq(NumT, BoolT)) = BoolT
  // constructorType("Either", "Left", Seq(NumT)) = a thrown Illtyped exception
  // constructorType("Either", "Right", Seq(BoolT)) = a thrown Illtyped exception
  // constructorType("Either", "Foo", Seq(UnitT)) = a thrown Illtyped exception
  // constructorType("Bar", "Left", Seq(UnitT)) = a thrown Illtyped exception
  //
  def constructorType(name: NameLabel, constructor: ConsLabel, types: Seq[Type]): Type = 
    (for {
      td <- typeDefs.find(_.name == name)
      rawType <- td.constructors.get(constructor)
      if (types.size == td.tvars.size)
    } yield replace(rawType, td.tvars.zip(types).toMap)).getOrElse(throw Illtyped)

  // Given a type and a mapping of type variables to other types, it
  // will recursively replace the type variables in `t` with the
  // types in `tv2t`, if possible.  If a type variable isn't
  // in `tv2t`, it should simply return the original type.  If a
  // `TFunT` is encountered, then whatever type variables it defines
  // (the first parameter in the `TFunT`) should overwrite whatever is in
  // `tv2t` right before a recursive `replace` call.  In other words,
  // type variables can shadow other type variables.
  //
  def replace( t:Type, tv2t:Map[TVar, Type] ): Type =
    t match {
      case NumT | BoolT | UnitT => ??? // FILL ME IN

      case FunT(params, ret) => ??? // FILL ME IN

      case RcdT(fields) => ??? // FILL ME IN

      case TypT(name, typs) => ??? // FILL ME IN

      case tv:TVar => ??? // FILL ME IN

      case TFunT(tvars, funt) => ??? // FILL ME IN
    }

  // HINT - the bulk of this remains unchanged from the previous assignment.
  // Feel free to copy and paste code from your last submission into here.
  def getType( e:Exp, env:TypeEnv ): Type =
    e match {
      case x:Var => ??? // FILL ME IN

      case _:Num => ??? // FILL ME IN

      case _:Bool => ??? // FILL ME IN

      case _:Unit => ??? // FILL ME IN

      case Plus | Minus | Times | Divide => ??? // FILL ME IN

      case LT | EQ => ??? // FILL ME IN

      case And | Or => ??? // FILL ME IN

      case Not => ??? // FILL ME IN

      case Fun(params, body) => ??? // FILL ME IN

      case Call(fun, args) => ??? // FILL ME IN

      case If(e1, e2, e3) => ??? // FILL ME IN

      case Let(x, e1, e2) => ??? // FILL ME IN

      case Rec(x, t1, e1, e2) => ??? // FILL ME IN

      case Record(fields) => ??? // FILL ME IN

      case Access(e, field) => ??? // FILL ME IN

      case c @ Construct(name, constructor, typs, e) => ??? // FILL ME IN

      case Match(e, cases) => ??? // FILL ME IN

      case TAbs(tvars, fun) => ??? // FILL ME IN

      case TApp(e, typs) => ??? // FILL ME IN
    }
}
