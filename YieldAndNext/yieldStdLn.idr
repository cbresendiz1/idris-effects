import Pipes
import Control.Monad.Trans

echo : IO ()
echo = ?hmm

deduplicating : (Eq a, Monad m) => Pipe a m a
deduplicating = recur (the (a -> Bool) (const True)) where
  recur isDifferent =
    awaitOne $ \x => do
      when (isDifferent x) (yield x)
      recur (/= x)

even : Monad m => Int -> Pipe String m String
even x = recur x where
     recur x =
         awaitOne $ \y => do
            yield (show x)
            recur (x + 2)


runnerGenerator : IO ()
runnerGenerator = runEffect $
    stdinLn "in> "
      .| takingWhile (/= "quit")
      .| even 0
      .| mapping ("out> " ++)
      .| stdoutLn

main : IO ()
main = runnerGenerator
