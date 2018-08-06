package bz

import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz.{Free, Monad, ~>}
import scalaz.Id.Id
import scalaz.std.option._

trait SubCategory[TF[_[_]], F[_], G[_]] {

  // defines a relationship from a tagless final algebra to a natural transformation
  // by target type
  val nt: (F ~> G)

  // defines a relationship from a natural transformation to a tagless final algebra
  // by target type
  val tf: TF[G]
}

trait TotalAlgebra[F[_], G[_], TF[_[_]]] {
  def interp(tf: TF[G]): (F ~> G)
}

object example extends App {

  trait Show[A]
  case class putStrLn(in: String) extends Show[Unit]

  implicit def showToFree[A](sop: Show[A]): Free[Show, A] = Free.liftF(sop)

  trait ShowAlg[F[_]] {
    def putStrLn(in: String): F[Unit]
  }

  implicit val showAlgId = new ShowAlg[Id] {
    def putStrLn(in: String): Id[Unit] = println(in)
  }

  implicit val showAlgOption = new ShowAlg[Option] {
    def putStrLn(in: String): Option[Unit] = Some(()).map(_ => println(in))
  }

  implicit def showInterp[F[_]](implicit S: ShowAlg[F]): (Show ~> F) = new (Show ~> F) {
    def apply[A](sa: Show[A]) = sa match {
      case putStrLn(in) => S.putStrLn(in)
    }
  }

  // TODO - how can i erase more info from this? parameterize Show/ShowAlg?
  implicit def totalAlgebra[F[_]]: TotalAlgebra[Show, F, ShowAlg] = new TotalAlgebra[Show, F, ShowAlg] {
    def interp(sa: ShowAlg[F]): (Show ~> F) = showInterp[F](sa)
  }

  implicit def mkInterp[F[_], G[_], TF[_[_]]](implicit ta: TotalAlgebra[F, G, TF], tf: TF[G]): (F ~> G) = ta.interp(tf)

  val prog = for {
    _ <- putStrLn("hi")
    _ <- putStrLn("hello")
    _ <- putStrLn("there")
  } yield "foo"

  implicit class FreeOps[F[_], A](prog: Free[F, A]) {
    def runTo[G[_]: Monad](implicit nt: (F ~> G)): G[A] = prog.foldMap(nt)
  }

  implicit val showId = mkInterp[Show, Id, ShowAlg]
  implicit val showOption = mkInterp[Show, Option, ShowAlg]
  prog.runTo[Id]
  prog.runTo[Option].foreach(println(_))
}
