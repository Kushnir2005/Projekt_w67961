module FinancialCalculator


let simpleInterest (principal: float) (rate: float) (time: float) =
    principal * (1.0 + rate * time)

let compoundInterest (principal: float) (rate: float) (timesPerYear: float) (years: float) =
    principal * (pown (1.0 + rate / timesPerYear) (int (timesPerYear * years)))


let annuityPayment (loanAmount: float) (rate: float) (periods: float) =
    if rate = 0.0 then loanAmount / periods
    else (loanAmount * rate) / (1.0 - (1.0 + rate) ** (-periods))

let main () =
    printf "Podaj kwotę początkową: "
    let principal = System.Double.Parse(System.Console.ReadLine())

    printf "Podaj roczną stopę procentową (np. 0,05 dla 5%%): "
    let rate = System.Double.Parse(System.Console.ReadLine())

    printf "Podaj liczbę lat: "
    let time = System.Double.Parse(System.Console.ReadLine())

    printfn "Odsetki proste: %f" (simpleInterest principal rate time)
    printfn "Odsetki złożone (roczna kapitalizacja): %f" (compoundInterest principal rate 1.0 time)

    printf "Podaj liczbę okresów kredytowych: "
    let periods = System.Double.Parse(System.Console.ReadLine())
    printfn "Rata kredytu: %f" (annuityPayment principal rate periods)

main()
