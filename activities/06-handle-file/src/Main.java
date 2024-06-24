import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        String[] input = scanner.nextLine().split(" ");
        int n1 = Integer.parseInt(input[0]);
        int n2 = Integer.parseInt(input[1]);
        int n3 = Integer.parseInt(input[2]);
        int n4 = Integer.parseInt(input[3]);

        // Leitura dos dados
        List<CountryData> countries = Files.lines(Paths.get("./dados.csv"))
                .map(CountryData::convertCSVToCountryData)
                .collect(Collectors.toList());

        // 1) A soma de "Active" de todos os países em que "Confirmed" é maior ou igual a n1
        int activeSumWhenConfirmedIsGreatherOrEqualOne = getActiveSumWhenConfirmedIsGreatherOrEqualOne(countries, n1);

        // 2) Dentre os n2 países com maiores valores de "Active", a soma das "Deaths" dos n3 países com menores valores de "Confirmed"
        int deathsSum = getDeathsSum(countries, n2, n3);

        // 3) Os n4 países com os maiores valores de "Confirmed". Os nomes devem estar em ordem alfabética.
        List<String> countriesWithHighestConfirmed = getCountriesWithHighestConfirmed(countries, n4);

        // Impressão dos resultados
        System.out.println(activeSumWhenConfirmedIsGreatherOrEqualOne);
        System.out.println(deathsSum);
        countriesWithHighestConfirmed.forEach(System.out::println);
    }

    private static int getActiveSumWhenConfirmedIsGreatherOrEqualOne(List<CountryData> countries, int n1) {
        return countries.stream()
                .filter(country -> country.confirmed >= n1)
                .mapToInt(country -> country.active)
                .sum();
    }

    private static int getDeathsSum(List<CountryData> countries, int n2, int n3) {
        return countries.stream()
                .sorted(Comparator.comparingInt(country -> -country.active))
                .limit(n2)
                .sorted(Comparator.comparingInt(country -> country.confirmed))
                .limit(n3)
                .mapToInt(country -> country.deaths)
                .sum();
    }

    private static List<String> getCountriesWithHighestConfirmed(List<CountryData> countries, int n4) {
        return countries.stream()
                .sorted(Comparator.comparingInt(country -> -country.confirmed))
                .limit(n4)
                .map(country -> country.name)
                .sorted()
                .collect(Collectors.toList());
    }
}

class CountryData {
    String name;
    int confirmed;
    int deaths;
    int recovered;
    int active;

    public CountryData(String name, int confirmed, int deaths, int recovered, int active) {
        this.name = name;
        this.confirmed = confirmed;
        this.deaths = deaths;
        this.recovered = recovered;
        this.active = active;
}

    public static CountryData convertCSVToCountryData(String line) {
        String[] parts = line.split(",");
        return new CountryData(
                parts[0],
                Integer.parseInt(parts[1]),
                Integer.parseInt(parts[2]),
                Integer.parseInt(parts[3]),
                Integer.parseInt(parts[4])
        );
    }
}