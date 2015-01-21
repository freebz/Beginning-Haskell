--
-- Ad-hoc Polymorphism: Type Classes
--


--
-- Declaring Classes and Instances
--

class Nameable n where
  name :: n -> String

initial :: Nameable n => n -> Char
initial n = head (name n)
