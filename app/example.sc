
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._


trait BookRepository[F[_]] {
  def getBooksBy(user: User): Future[List[Book]]
}

object BookRepository {
  def apply[F[_]](implicit ev: BookRepository[Future]): BookRepository[Future] = ev
}



trait UserRepository[F[_]] {
  def getUser(name: String): Future[Option[User]]
  def deleteUser(userId: Int): Future[Int]
}

object UserRepository {
  def apply[F[_]](implicit ev: UserRepository[F]): UserRepository[F] = ev
}



trait UserService[F[_]] {
  import UserService._

  def delUser(userName: String): Future[ErrorOr[Ok]]
  def anyBooksFor(user: User): Future[Boolean]
  def getUser(name: String): Future[Option[User]]
}

object UserService { self: BookRepository[Future] =>
  type ErrorOr[A] = Either[NotFound, A]

  def apply[F[_]](implicit ev: UserService[Future]): UserService[Future] = ev

  def instance[F[_] : UserRepository]: UserService[Future] = new UserService[Future] {

    override def anyBooksFor(user: User): Future[Boolean] =
      self.getBooksBy(user)
        .map(_.isEmpty)


    private def check(user: User): Future[List[Book]] = for {
      books <- self.getBooksBy(user)
    } yield books


    override def delUser(userName: String): Future[ErrorOr[Ok]] =
    for {
      maybeUser <- UserRepository[F].getUser(userName)
      books     <- maybeUser.fold(Future(List.empty[Book]))(usr => check(usr))

      result = maybeUser match {
        case None      => Left(UserNotFound(userName))
        case Some(usr) =>
          if (books.isEmpty) {
            UserRepository[F].deleteUser(usr.id)
            Right(UserDeleted)
          }
          else Left(UserHasBooks(books))
      }
    } yield result

    override def getUser(name: String): Future[Option[User]] =
      UserRepository[F].getUser(name)
  }

}







case class User(name: String, email: String, id: Int)
case class Book(isbn: Int, description: String)


sealed trait NotFound extends Product with Serializable
case class UserNotFound(name: String) extends NotFound
case class UserHasBooks(books: List[Book]) extends NotFound
case object UserHasBooks extends NotFound




sealed trait Ok
case object UserDeleted extends Ok