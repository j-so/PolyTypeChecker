import scala.io._
import cs162.assign3.syntax._
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

      case Right(program) => // correctly parsed
        Checker(program.typedefs).getType(program.e, TypeEnv())
        println("This program is well-typed.")
    }
  }
}

case class Checker(typeDefs: Set[TypeDef]) {
  import Checker.{ TypeEnv, Illtyped }

  // Gets all the constructors associated with a given type name.
  // For example, consider the following typedefs:
  //
  // type Either = Left num | Right bool
  // type Maybe = Some num | None
  //
  // With respect to the above typedefs, `constructors` will return
  // the following for the given example arguments:
  //
  // constructors(NameLabel("Either")) =
  //   Map(ConsLabel("Left") -> NumT, ConsLabel("Right") -> BoolT)
  //
  // constructors(NameLabel("Maybe")) =
  //   Map(ConsLabel("Some") -> NumT, ConsLabel("None") -> UnitT)
  //
  // constructors(NameLabel("Fake")) will throw the Illtyped exception
  //
  def constructors(name: NameLabel): Map[ConsLabel, Type] =
    typeDefs.
      find(_.name == name).
      map(_.constructors).
      getOrElse(throw Illtyped)

  // Type-check the given expression using the given type environment;
  // if successful returns the expression's type, if not successful
  // throws an Illtyped exception.
  def getType( e:Exp, env:TypeEnv ): Type =
    e match {
      // variables
      case x:Var => env.getOrElse(x, throw Illtyped)

      // numeric literals
      case _:Num => NumT

      // boolean literals
      case _:Bool => BoolT

      // `nil` - the literal for unit
      case _: NilExp => UnitT

      // builtin arithmetic operators
      case Plus | Minus | Times | Divide => FunT(Seq(NumT, NumT), NumT)

      // builtin relational operators
      case LT | EQ => FunT(Seq(NumT, NumT), BoolT)

      // builtin logical operators
      case And | Or => FunT(Seq(BoolT, BoolT), BoolT)

      // builtin logical operators
      case Not => FunT(Seq(BoolT), BoolT)

      // function creation
      case Fun(params, body) =>
        FunT(params.map(s => s._2), getType(body, env ++ params.toMap))

      // function call
      case Call(fun, args) =>
        getType(fun, env) match {
          case FunT(params:Seq[Type], ret:Type) =>
            if(params == args.map((e: Exp) => getType(e, env))) ret
            else throw Illtyped
          case _ => throw Illtyped
        }

      // conditionals 
      case If(e1, e2, e3) =>
        getType(e1, env) match {
          case BoolT =>
            if(getType(e2, env) == getType(e3, env)) getType(e3, env)
            else throw Illtyped
          case _ => throw Illtyped
        }

      // let binding
      case Let(x, e1, e2) =>
        getType(e2, env + (x -> getType(e1, env)))

      // recursive binding
      case Rec(x, t1, e1, e2) =>
        t1 match {
          case FunT(params, ret) =>
            getType(e1, env + (x -> t1)) match {
              case FunT(params1, ret1) =>
                if (ret == ret1) getType(e1, env + (x -> t1))
                else{
                  ret1 match {
                    case TypT(name) =>
                      if(constructors(name).values.exists(_ == ret)) getType(e1, env + (x -> t1))
                      else throw Illtyped
                    case _ => throw Illtyped
                  }
                }
              case _ =>
                if (ret == getType(e1, env + (x -> t1)))
                  getType(e2, env + (x -> t1))
                else throw Illtyped
            }
          case _ =>
            if(t1 == getType(e1, env + (x -> t1))) t1
            else throw Illtyped
        }

      // record literals
      case Record(fields) =>
        RcdT(fields.map(fe => (fe._1, getType(fe._2, env))))

      // record access
      case Access(e, field) =>
        getType(e, env) match {
          case RcdT(fields) => fields.getOrElse(field, throw Illtyped)
        }

      // constructor use
      case Construct(name, constructor, e) =>
        if (constructors(name).getOrElse(constructor, throw Illtyped) == getType(e, env)) TypT(name)
        else {
          throw Illtyped
        }

      // pattern matching (case ... of ...)
      case Match(e, cases) =>
        if(cases.isEmpty) throw Illtyped
        getType(e, env) match {
          case TypT(name) =>
            if(cases.size != constructors(name).size) throw Illtyped
            if(cases.map(c => c._1).distinct.size != cases.size) throw Illtyped
            if(cases.map(c => getType(c._3, env + (c._2 -> constructors(name).getOrElse(c._1, throw Illtyped)))).distinct.size != 1) throw Illtyped
            TypT(name)
        }
    }
}
