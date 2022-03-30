module Andromeda.Common.Physics where

-- These definitions are not accurate, they don't represent
-- real physical units! It's just dummies for demo.

data Period = Secondly
  deriving (Show, Eq, Ord)


newtype Temperature = Kelvin Float
  deriving (Show, Eq, Ord)

newtype Pressure = Pascal Float
  deriving (Show, Eq, Ord)

newtype Mass = Kilogram Float
  deriving (Show, Eq, Ord)

newtype Velocity = MetersPerSecond Float
  deriving (Show, Eq, Ord)

newtype TimeInterval = Seconds Float
  deriving (Show, Eq, Ord)

newtype Angle = Radian Float
  deriving (Show, Eq, Ord)

newtype Frequency = Hertz Int
  deriving (Show, Eq, Ord)




newtype Thrust = Thrust Float
  deriving (Show, Eq, Ord)

newtype Torque = Torque Float
  deriving (Show, Eq, Ord)

newtype Force = Force Float
  deriving (Show, Eq, Ord)

newtype Impulse = Impulse Float
  deriving (Show, Eq, Ord)

newtype AngularImpulse = AngularImpulse Float
  deriving (Show, Eq, Ord)


data PhysicalUnit
  = UnitTemperature Temperature
  | UnitPressure Pressure
  | UnitMass Mass
  | UnitVelocity Velocity
  | UnitThrust Thrust
  | UnitTorque Torque
  | UnitAngle Angle
  deriving (Show, Eq, Ord)
