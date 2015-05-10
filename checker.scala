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
      case NumT | BoolT | UnitT => t

      case FunT(params, ret) =>
        FunT(params.map(a => replace(a, tv2t)), replace(ret, tv2t))

      case RcdT(fields) =>
        RcdT(fields.map(fe => (fe._1, replace(fe._2, tv2t))))

      case TypT(name, typs) =>
        TypeT(name, typs.map(a => replace(a, tv2t)))

      case tv:TVar => tv2t.getOrElse(tv, t)

      case TFunT(tvars, funt) => replace(funt, tv2t ++ tvars.zip(funt.params).toMap)
    }

  // HINT - the bulk of this remains unchanged from the previous assignment.
  // Feel free to copy and paste code from your last submission into here.
  def getType( e:Exp, env:TypeEnv ): Type =
    e match {
      case x:Var => env.getOrElse(x, throw Illtyped)

      case _:Num => NumT

      case _:Bool => BoolT

      case _:Unit => NilT

      case Plus | Minus | Times | Divide => FunT(Seq(NumT, NumT), NumT)

      case LT | EQ => FunT(Seq(NumT, NumT), BoolT)

      case And | Or => FunT(Seq(BoolT, BoolT), BoolT)

      case Not => FunT(Seq(BoolT), BoolT)

      case Fun(params, body) =>
        FunT(params.map(s => s._2), getType(body, env ++ params.toMap))

      case Call(fun, args) =>
        getType(fun, env) match {
          case FunT(params:Seq[Type], ret:Type) =>
            if(params == args.map((e: Exp) => getType(e, env))) ret
            else throw Illtyped
          case _ => throw Illtyped
        }

      case If(e1, e2, e3) =>
        getType(e1, env) match {
          case BoolT =>
            if(getType(e2, env) == getType(e3, env)) getType(e3, env)
            else throw Illtyped
          case _ => throw Illtyped
        }

      case Let(x, e1, e2) =>
        getType(e2, env + (x -> getType(e1, env)))

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

      case Record(fields) =>
        RcdT(fields.map(fe => (fe._1, getType(fe._2, env))))

      case Access(e, field) =>
        getType(e, env) match {
          case RcdT(fields) => fields.getOrElse(field, throw Illtyped)
          case _ => throw Illtyped
        }

      case c @ Construct(name, constructor, typs, e) =>
        if(constructorType(name, constructor, typs) == getType(e, env)) TypT(name, typs)
        else throw Illtyped

      case Match(e, cases) =>
        if(cases.isEmpty) throw Illtyped
        getType(e, env) match {
          case a:TypT(name, typs) =>
            if(cases.size != constructors(name).size) throw Illtyped
            if(cases.map(c => c._1).distinct.size != cases.size) throw Illtyped
            if(cases.map(c => getType(c._3, env + (c._2 -> constructorType(name, c._1, typs)))).distinct.size != 1) throw Illtyped
            TypT(name)
          case _ => throw Illtyped
        }

      case TAbs(tvars, fun) =>
        getType(fun, env) match {
          case f:FunT(params, body) =>
            TFunT(tvars, f)
          case _ => throw Illtyped
        }

      case TApp(e, typs) =>
        getType(e, env) match {
          case f:TFunT(tvars, funt) =>
            replace(f, Map())
          case _ => throw Illtyped
        }
    }
}
