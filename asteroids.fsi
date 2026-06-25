module Asteroids

type Move =
    new : initialX:float * initialY:float -> Move
    member x : float
    member y : float
    member length : float
    static member add : Move * Move -> Move
    static member scale : float * Move -> Move

type BaseClass =
    new : position: Move * speed: Move * radius: float -> BaseClass
    member position : Move
    member speed : Move
    member radius : float
    static member time_step : float

type Asteroid =
    inherit BaseClass
    new : Move * Move * float -> Asteroid
    member CheckSpeed : unit -> unit
    static member CreateAsteroid : Move * Move * float -> Asteroid

type Spaceship =
    inherit BaseClass
    new : Move * Move * float * Move -> Spaceship
    member orientation : Move with get, set
    member acceleration : Move with get, set

type Bullet = 
    inherit BaseClass
    new : position: Move * speed: Move * radius: float * lifetime: float -> Bullet
    member updateBullet : float -> unit
    member lifetime : float

[<AbstractClass; Sealed>]
type Collide =
    static member BulletsAsteroid : Bullet list * Asteroid list -> Asteroid list * Bullet list
    static member SpaceShipAsteroid : Spaceship * Asteroid list -> bool
    static member AsteroidAsteroid : Asteroid list -> Asteroid list * Asteroid list
    static member SpaceShipBullet : Spaceship * Bullet list -> bool
