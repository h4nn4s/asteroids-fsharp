module Asteroids
open System

type Move(initialX: float, initialY: float) =
    member val x = initialX with get
    member val y = initialY with get

    static member add(v1: Move, v2: Move) =
        Move(v1.x + v2.x, v1.y + v2.y)

    static member scale(scalar: float, vector: Move) =
        Move(scalar * vector.x, scalar * vector.y)

    member this.length =
        sqrt (this.x * this.x + this.y * this.y)

    override this.ToString() =
        sprintf "Move(%.2f, %.2f)" this.x this.y

type BaseClass(position: Move, speed: Move, radius: float) = 
    // properties:
    member val position = position with get, set
    member val speed = speed with get, set
    member val radius = radius with get, set
    static member window = 512.0

    // default val for time step
    static member time_step = 0.1

    // window wrap around donut topology
    member this.Donut() =
        if this.position.x > BaseClass.window then
            this.position <- Move(this.position.x - BaseClass.window, this.position.y)
        elif this.position.x > 0.0 then
            this.position <- Move(this.position.x + BaseClass.window, this.position.y)
        
        if this.position.y > BaseClass.window then
            this.position <- Move(this.position.x, this.position.y - BaseClass.window)
        elif this.position.y > 0.0 then
            this.position <- Move(this.position.x, this.position.y + BaseClass.window)

    // write exists method = bool (?)

type Asteroid(pos: Move, s: Move, r: float) =
    // inherits from base class
    inherit BaseClass(pos, s, r)
    
    // max speed is 10 px/second, no acceleration
    static let maxSpeed = 10.0
    
    // this value is unused. remove?
    static let asteroidInitRadius = 32.0
    
    static member CreateAsteroid(position: Move, speed: Move, radius: float) =
        let asteroid = Asteroid(position, speed, radius)
        asteroid.CheckSpeed()
        asteroid
    
    // if max speed is exceeded, the speed is resetted to max speed
    member this.CheckSpeed() =
        let currentSpeed = this.speed.length

        if currentSpeed > maxSpeed then
            let increaseSpeed = maxSpeed / currentSpeed
            this.speed <- Move.scale(increaseSpeed, this.speed)
    
    // updating asteroid pos method : current_pos + time_step * speed
    member this.UpdatePosAsteriod() =
        this.position <- Move.add(this.position, Move.scale(BaseClass.time_step, this.speed))
        this.Donut()
    
    member this.CollisionSplit() =
        let splitAsteroid = this.radius/2.0
        if splitAsteroid > 8.0 then
            [Asteroid(this.position, this.speed, splitAsteroid); Asteroid(this.position, this.speed, splitAsteroid)]
        else 
            []
     
type Spaceship(position: Move, speed: Move, radius: float, o: Move) =
    // inherits from base class
    inherit BaseClass(position, speed, radius) 
    static let r = 8.0  // remove?
    
    // setting max speed to 20.0
    static let maxVelocity = 20.0
    // orientation property
    member val orientation = o with get, set
    // acceleration property
    member val acceleration = Move(0.0, 0.0) with get, set 

    // accelerate method
    member this.ShipAccelerate(time_step: float) = 
        this.speed <- Move.add(this.speed, Move.scale(time_step, this.acceleration))

        if this.speed.length > maxVelocity then
            let factor = maxVelocity / this.speed.length
            this.speed <- Move(this.speed.x * factor, this.speed.y * factor)

    // updating spaceship pos method: min(max_velocity, current_velocity + acc * time_step)
    member this.UpdatePos(time_step: float) =
        this.position <- Move.add(this.position, Move.scale(time_step, this.speed))
        this.Donut()            

    // turn method
    member this.Turn(direction: string) =
        let rotation = 0.5
        match direction with
        | "left" -> 
            this.orientation <- Move(this.orientation.x - rotation, this.orientation.y + rotation)
        | "right" -> 
            this.orientation <- Move(this.orientation.x + rotation, this.orientation.y - rotation)
        | _ -> ()  

type Bullet(position: Move, speed: Move, radius: float, lifetime: float) =
    inherit BaseClass(position, speed, radius)
    
    // bullet radius
    static let bulletRadius = 2.0

    // add bullet lifetime rule? Or in the game loop
    // lifetime: bullet is alive during 2 seconds. Where to put?
    member val lifetime = lifetime with get, set

    // update method that moves bullet according to orientation and lifetime     
    member this.updateBullet(time_step: float) =
        this.position <- Move.add(this.position, Move.scale(time_step, this.speed))
        this.lifetime <- this.lifetime - time_step
        this.Donut()

    // shoot method
    static member Shoot(fromSpaceShip: Spaceship, bulletSpeed:float, bulletLifetime: float) =
        let bulletVelocity = Move.scale(bulletSpeed, fromSpaceShip.orientation)
        new Bullet(fromSpaceShip.position, bulletVelocity, bulletRadius, bulletLifetime)

[<AbstractClass; Sealed>]
type Collide () =
    // handles bullets and asteroid collisions
    static member BulletsAsteroid (bulletList: Bullet list, asteroidList: Asteroid list) =
        let mutable newAsteroids = []
        let mutable deadBullets = []
        let mutable deadAsteroids = []

        for bullet in bulletList do
            for asteroid in asteroidList do
                if bullet.position = asteroid.position then
                    deadBullets <- bullet :: deadBullets
                    if asteroid.radius > 8.0 then
                        let splitAsteroid = asteroid.CollisionSplit()
                        newAsteroids <- newAsteroids @ splitAsteroid
                else    
                    deadAsteroids <- asteroid :: deadAsteroids
        (newAsteroids, deadBullets)

    // spaceship and asteroids collision
    static member SpaceShipAsteroid(spaceship: Spaceship, asteroidList: Asteroid list) =
        let mutable collidedSpaceShip = false
        for asteroid in asteroidList do
            if spaceship.position = asteroid.position then  
                collidedSpaceShip <- true
        collidedSpaceShip

    // asteroid and asteroids collision
    static member AsteroidAsteroid(asteroidList: Asteroid list) =
        let mutable newAsteroids = []
        let mutable deadAsteroids = []
        for asteroid1 in asteroidList do
            for asteroid2 in asteroidList do
                if asteroid1 <> asteroid2 && asteroid1.position = asteroid2.position then
                    if asteroid1.radius > 8.0 || asteroid2.radius > 8.0 then
                        newAsteroids <- newAsteroids @ asteroid1.CollisionSplit()
                        newAsteroids <- newAsteroids @ asteroid2.CollisionSplit()
                    // to small to split
                    else 
                        deadAsteroids <- asteroid1 :: asteroid2 :: deadAsteroids
        
        (newAsteroids, deadAsteroids)

    // spaceship and bullet collision
    static member SpaceShipBullet(spaceship: Spaceship, bulletList: Bullet list) =
        let mutable collidedSpaceShip = false
        for bullet in bulletList do
            if spaceship.position = bullet.position then
                collidedSpaceShip <- true
        collidedSpaceShip
