import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (foldl')
import Data.Array.IArray(Array, array, (!), (//))
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split tok arr =
    if not (null y) then
        x:(split tok newArr)
    else
        [x]
    where
        (x, y) = break (==tok) arr
        newArr = tail y

union x = foldl' S.union S.empty x


join _ [] = []
join tok (x:arr) = x ++ (if null arr then [] else ([tok] ++ join tok arr))

applyInstruction (acc, pointer) (op, delta) =
    case op of
        "acc" -> (acc + delta, pointer + 1)
        "jmp" -> (acc, pointer + delta)
        "nop" -> (acc, pointer + 1)

parseDelta d = if head d == '+' then read (tail d) else read d

parseInstruction :: String -> (String, Int)
parseInstruction str = (op, delta)
    where
        [op, deltaStr] = split ' ' str
        delta = parseDelta deltaStr

runInstructionOnce :: Array Int Instruction -> ProgramState -> ProgramState
runInstructionOnce instructions (pointersVisited, acc, pointer, exited, duplicate) = (newPointers, newAcc, newPointer, newExited, newDuplicate)
    where
        newPointers = S.insert pointer pointersVisited
        (newAcc, newPointer) = if exited then (acc, pointer) else applyInstruction (acc, pointer) (instructions ! pointer)
        newExited = newPointer >= length instructions
        newDuplicate = S.member newPointer pointersVisited


type Instruction = (String, Int)
type ProgramState = (S.Set Int, Int, Int, Bool, Bool)

runProgramOnce :: Array Int Instruction -> ProgramState
runProgramOnce instructions = head (dropWhile (\(_, _, _, exited, duplicate) -> (not exited) && (not duplicate)) (iterate (runInstructionOnce instructions) (S.empty, 0, 0, False, False)))

swapInstr (op, delta) = case op of
    "acc" -> ("acc", delta)
    "jmp" -> ("nop", delta)
    "nop" -> ("jmp", delta)


main = do
    contents <- readFile "inputs/8.txt"
    let instructionsList = map parseInstruction (lines contents)
    let instructions = array (0, ((length instructionsList) - 1)) (zip [0..] instructionsList)
    let (__, part1, _, _, _) = runProgramOnce instructions

    let instructionSets = [ instructions // [(p, (swapInstr instr))] | (p, instr) <- zip [0..] instructionsList]
    let (_, part2, _, _, _) = head (filter (\(_, _, _, exited, _) -> exited) [runProgramOnce s | s <- instructionSets])

    print (part1, part2)