public class Pet {
    int age;
    float weight;
    float height;
    String color;

    public void sleep(){
        System.out.println(
            "Good night, see you tomorrow");
    }
    public void eat(){
        System.out.println(
            "I'm so hungry...let me have a snack like nachos!");
    }
    public String say(String aWord){
        String petResponse = "OK!! OK!! " +aWord;
        return petResponse;
    }

    public int age() {
        int age = 9
        return age;
    }
}

