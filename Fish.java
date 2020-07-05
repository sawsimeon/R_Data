public class Fish extends Pet {

    int currentDepth=0;
  
    public int dive(int howDeep){
      currentDepth=currentDepth + howDeep;
        if (currentDepth > 100){
            System.out.println("I am a little fish and "
                        + " can't dive below 100 feet");
            currentDepth=currentDepth - howDeep;
        }else{
          System.out.println("Diving for " + howDeep +
                                               " feet");
          System.out.println("I'm at " + currentDepth +
                          " feet below the sea level");
        }
        return currentDepth;
    }
  
    public String talk(String something){
         return "Don't you know that fish do not talk?";
    }
  }