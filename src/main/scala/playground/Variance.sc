/*
  Big rule
  - method arguments are in CONTRAVARIANT position
  - return types are in COVARIANT position
 */

/**
 * 1. Invariant, covariant, contravariant
 *   Parking[T](things: List[T]) {
 *     park(vehicle: T)
 *     impound(vehicles: List[T])
 *     checkVehicles(conditions: String): List[T]
 *   }
 *
 * 2. used someone else's API: IList[T]
 * 3. Parking = monad!
 *     - flatMap
 */
class Vehicle
class Bike extends Vehicle
class Car extends Vehicle
class IList[T]

// Invariant APIs
class IParking[T](vehicles: List[T]) {
  def park(vehicle: T): IParking[T] = ???
  def impound(vehicles: List[T]): IParking[T] = ???
  def checkVehicles(conditions: String): List[T] = ???
  def flatMap[S](f: T => IParking[S]): IParking[S] = ???
}

// Covariant APIs
class CParking[+T](vehicles: List[T]) {
  def park[S >: T](vehicle: S): CParking[S] = ???
  def impound[S >: T](vehicles: List[S]): CParking[S] = ???
  def checkVehicles(conditions: String): List[T] = ???
  def flatMap[S](f: T => CParking[S]): CParking[S] = ???
}

// Contravariant APIs
class XParking[-T](vehicles: List[T]) {
  def park(vehicle: T): XParking[T] = ???
  def impound(vehicles: List[T]): XParking[T] = ???
  def checkVehicles[S <: T](conditions: String): List[S] = ???
  def flatMap[R <: T, S](f: R => XParking[S]): XParking[S] = ???
}

/*
  Rule of thumb
  - use covariance = COLLECTION OF THINGS
  - use contravariance = GROUP OF ACTIONS
 */

class CParking2[+T](vehicles: IList[T]) {
  def park[S >: T](vehicle: S): CParking2[S] = ???
  def impound[S >: T](vehicles: IList[S]): CParking2[S] = ???
  def checkVehicles[S >: T](conditions: String): IList[S] = ???
}

class XParking2[-T](vehicles: IList[T]) {
  def park(vehicle: T): XParking2[T] = ???
  def impound[S <: T](vehicles: IList[S]): XParking2[S] = ???
  def checkVehicles[S <: T](conditions: String): IList[S] = ???
}