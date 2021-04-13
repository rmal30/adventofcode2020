import qualified Data.Set as S
import Data.Array.IArray(Array, array, (!), (//))
import Utils(split)

data Operation = Accumulate | Jump | NoOperation
type Instruction = (Operation, Int)
type ProgramState = (Int, Int)
type ExecutionState = (S.Set Int, ProgramState, Bool, Bool)

applyInstruction :: ProgramState -> Instruction -> ProgramState
applyInstruction (accumulator, pointer) (Accumulate, delta) = (accumulator + delta, pointer + 1)
applyInstruction (accumulator, pointer) (Jump, delta) = (accumulator, pointer + delta)
applyInstruction (accumulator, pointer) (NoOperation, _) = (accumulator, pointer + 1)

parseDelta :: String -> Int
parseDelta ('+':d) = read d
parseDelta ('-':d) = -(read d)
parseDelta _ = 0

instance Read Operation where
    readsPrec _ "acc" = [(Accumulate, "")]
    readsPrec _ "jmp" = [(Jump, "")]
    readsPrec _ "nop" = [(NoOperation, "")]
    readsPrec _  _    = []

parseInstruction :: String -> Instruction
parseInstruction str = (read operationStr, parseDelta deltaStr)
    where
        [operationStr, deltaStr] = split ' ' str

runInstructionOnce :: Array Int Instruction -> ExecutionState -> ExecutionState
runInstructionOnce instructions (pointersVisited, (accumulator, pointer), exited, _) = (newPointers, (newAccumulator, newPointer), newExited, newDuplicate)
    where
        newPointers = S.insert pointer pointersVisited
        (newAccumulator, newPointer) = 
            if exited then 
                (accumulator, pointer) 
            else 
                applyInstruction (accumulator, pointer) (instructions ! pointer)
        newExited = newPointer >= length instructions
        newDuplicate = S.member newPointer pointersVisited

runProgramOnce :: Array Int Instruction -> ExecutionState
runProgramOnce instructions = lastProgramState
    where
        programStates = iterate (runInstructionOnce instructions) (S.empty, (0, 0), False, False)
        lastProgramState:_ = dropWhile (\(_, _, exited, duplicate) -> not exited && not duplicate) programStates

swapOp :: Operation -> Operation
swapOp Accumulate = Accumulate
swapOp Jump = NoOperation
swapOp NoOperation = Jump

main :: IO ()
main = do
    contents <- readFile "inputs/8.txt"
    let instructionsList = map parseInstruction (lines contents)
    let instructions = array (0, length instructionsList - 1) (zip [0..] instructionsList)
    let (_, (part1, _), _, _) = runProgramOnce instructions

    let instructionSets = [ instructions // [(pointer, (swapOp operation, value))] | (pointer, (operation, value)) <- zip [0..] instructionsList]
    let (_, (part2, _), _, _) = head (filter (\(_, _, exited, _) -> exited) [runProgramOnce modifiedInstructions | modifiedInstructions <- instructionSets])

    print (part1, part2)