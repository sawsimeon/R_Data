public class TemperatureConverter {
    public static float convertTemp1 (float temperature,
        char convertTo) {
        return convertTo;}
        public static String convertTemp (float temperature, char convertTo) {
    
         if (convertTo=='F'){
         return "The temperature in Fahrenheit is " + (9*temperature/5 + 32);
        } else if(convertTo=='C') {
         return "The temperature in Celsius is " + (temperature - 32) * 5/9;
        }  else{
         return "You can enter either F or C as convertTo argument";
    
    }
    }
    
    public static void main(String[] args) {
    
    System.out.println("Converting 21C to Fahrenheit. " +  convertTemp(21,'F', 0));
    System.out.println("Converting 70F to Celsius. " + convertTemp(70,'C', 0));
    }
    }