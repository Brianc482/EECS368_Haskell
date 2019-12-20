import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.List
import Data.Char


currentMonth = 4
currentYear = 1982

printCal :: IO()
printCal = do
       putStrLn $ formCalendar calCells

months = ["January",
          "February",
          "March",
          "April",
          "May",
          "June",
          "July",
          "August",
          "September",
          "October",
          "November",
          "December"]
 
concatenate :: [[a]] -> [a]
concatenate [] = []
concatenate (x:xs) = x ++ concatenate xs

chunk n xs = chunk' i xs
      where 
        chunk' _ [] = []
        chunk' n xs = a : chunk' n b where (a,b) = splitAt n xs

        i = ceiling (fromIntegral (length xs) / fromIntegral n)

monthAsString = months!!(currentMonth-1)
monthDate = totalDaysInMonth currentYear currentMonth
startDate = snd(monthDate)
totalDays = fst(monthDate)
monthLine = "\t" ++ concatenate [[x] ++ "" | x <- monthAsString]
yearsLine = " " ++ show currentYear
daysLine = "\n Su Mo Tu We Th Fr Sa\n"
displayCalendar = concatenate[monthLine, yearsLine, daysLine]
 
totalDaysInMonth currentYear currentMonth = (numberOfDays, startDate) 
                      where numberOfDays = gregorianMonthLength currentYear currentMonth
                            startDate = digitToInt(last(showWeekDate (fromGregorian currentYear currentMonth 01)))

padding number str = str ++ (replicate (number - (length str)) ' ')
calCells = (replicate cell1 "") ++ [ show d | d <- [1..lastday] ] ++ (replicate cell2 "")
          where lastday = totalDays
                cell1 = startDate
                cell2 = (7-(totalDays-(29-startDate)-1))

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

formCell cell = " " ++ padding 2 cell 

formChunk chunk = (concatenate $ map formCell chunk) ++ "\n"

formCalendar calCells =  displayCalendar ++ concatenate(map formChunk chunks)
  where chunks = splitEvery 7 calCells
