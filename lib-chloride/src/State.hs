{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Redundant bracket" #-}
module State (State, getState, stateFrom, runState) where

data State s a = State (s -> (s, a))

instance Functor (State s) where
    -- create a State where the return value will have
    -- a function applied to it before being outputed
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State transform) = State (fmap f . transform) -- fmap on tuples only applies to the 2nd value
        -- nextStateFunc state s
        --     = (state', f val)
        --     where (state', val) = runState state s

instance Applicative (State s) where
    -- basically the 'const' of states: given any state,
    -- we'll always return val
    pure :: a -> State s a
    pure = State . flip (,) -- \val -> \s -> (s, val)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) funcState valueState
        -- create a function that takes in the initial state and then:
        --      1. runs funcState after s0 and collects the returned function
        --      2. runs valueState "after" running funcState, i.e. using
        --         the new state we got to when collecting f
        --      3. returns the new state/value pair we got from valueState
        = State newTransform
        where
            newTransform s0 = (s2, f val2)
                where (s1, f)    = runState funcState s0
                      (s2, val2) = runState valueState s1

instance Monad (State s) where
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b
    valState >>= fromValToState
        = State newTransform
        where
            -- In an initial state s0,
            --      1. run the valueState to get into state s1
            --      2. use the returned value to get a new State object
            --      3. run the State's transform in the context of s1
            --      4. return the final state/value pair
            newTransform s0 = (s2, b)
                where (s1, a) = runState valState s0
                      funcState = fromValToState a
                      (s2, b) = runState funcState s1

-- | A 'State' object that will also return the current state as its value
getState :: State s s
getState = State (\s -> (s, s))

-- | Creates a new 'State' object that will always have the given state
stateFrom :: s -> State s ()
stateFrom = State . const (,()) -- \s -> \_ -> (s, ())

runState :: State s a -> s -> (s, a)
runState (State stateFunc) = stateFunc