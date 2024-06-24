import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        String[] input = scanner.nextLine().split(" ");
        int n1 = Integer.parseInt(input[0]);
        int n2 = Integer.parseInt(input[1]);
        int n3 = Integer.parseInt(input[2]);
        int n4 = Integer.parseInt(input[3]);

        // Data read
        List<CountryData> countries = readCountryDataFromFile("./dados.csv");

        // 1) A soma de "Active" de todos os países em que "Confirmed" é maior ou igual a n1
        int activeSumWhenConfirmedIsGreatherOrEqualOne = getActiveSumWhenConfirmedIsGreatherOrEqualOne(countries, n1);

        // 2) Dentre os n2 países com maiores valores de "Active", a soma das "Deaths" dos n3 países com menores valores de "Confirmed"
        int deathsSum = getDeathsSum(countries, n2, n3);

        // 3) Os n4 países com os maiores valores de "Confirmed". Os nomes devem estar em ordem alfabética.
        List<String> countriesWithHighestConfirmed = getCountriesWithHighestConfirmed(countries, n4);

        // Show results
        System.out.println(activeSumWhenConfirmedIsGreatherOrEqualOne);
        System.out.println(deathsSum);

        for (String country : countriesWithHighestConfirmed) {
            System.out.println(country);
        }
    }

    private static List<CountryData> readCountryDataFromFile(String fileName) throws IOException {
        List<CountryData> countries = new ArrayList<>();
        BufferedReader reader = new BufferedReader(new FileReader(fileName));
        String line;

        while ((line = reader.readLine()) != null) {
            CountryData countryData = CountryData.convertCSVToCountryData(line);
            countries.add(countryData);
        }

        reader.close();
        return countries;
    }

    private static int getActiveSumWhenConfirmedIsGreatherOrEqualOne(List<CountryData> countries, int n1) {
        int sum = 0;

        for (CountryData country : countries) {
            if (country.confirmed >= n1) {
                sum += country.active;
            }
        }
        return sum;
    }

    private static int getDeathsSum(List<CountryData> countries, int n2, int n3) {
        int deathsSum = 0;

        // Order by active in descending order
        countries.sort(Comparator.comparingInt(c -> -c.active));

        // Filter the top n2 countries with the highest "Active" values
        List<CountryData> topActiveCountries = countries.subList(0, Math.min(n2, countries.size()));

        // Order by confirmed in ascending order
        topActiveCountries.sort(Comparator.comparingInt(c -> c.confirmed));

        // Iterate over the top n3 countries with the lowest "Confirmed" values
        for (int i = 0; i < Math.min(n3, topActiveCountries.size()); i++) {
            deathsSum += topActiveCountries.get(i).deaths;
        }

        return deathsSum;
    }

    private static List<String> getCountriesWithHighestConfirmed(List<CountryData> countries, int n4) {
        List<String> result = new ArrayList<>();
        int count = 0;

        countries.sort(Comparator.comparingInt(c -> -c.confirmed));

        for (CountryData country : countries) {
            // If the number of countries is greater than n4, stop the loop
            if (count >= n4) {
                break;
            }
            result.add(country.name);
            count++;
        }
        Collections.sort(result);
        return result;
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