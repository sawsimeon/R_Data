public class FishMaster {
    public static void main(String[] args) {
        Fish myFish = new Fish();

        // Try to have fish go below 100 feet

        myFish.dive(2); // go 2 feet down
        myFish.dive(97); // go another 9 feet down
        myFish.dive(3); // go 3 more feet down

        myFish.sleep();
    }
}