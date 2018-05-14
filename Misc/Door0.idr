data DoorState = OPEN | CLOSED

data DoorH : DoorState -> UniqueType where
     MkDH : DoorH s
     
data DoorCmd : Type* -> Type* where
     Open : DoorH CLOSED -> DoorCmd (DoorH OPEN)
     Knock : DoorH CLOSED -> DoorCmd (DoorH CLOSED)
     Close : DoorH OPEN -> DoorCmd (DoorH CLOSED)
     
data DoorLang : Type* -> Type* where
     Return : {a : Type*} -> a -> DoorLang a
     Action : DoorCmd a -> DoorLang a
     (>>=) : DoorLang a -> (a -> DoorLang b) -> DoorLang b
     
testProg : DoorH CLOSED -> DoorLang ()
testProg h = do h <- Action (Knock h)
                h <- Action (Open h)
                h <- Action (Close h)
                Return ()
