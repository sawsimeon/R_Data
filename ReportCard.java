public class ReportCard {
    String studentName;

    /**
     The method converGrads has one integer argument - the result of the school test. The method returns
     one letter A, B, C or D depending on the argument's value.
     */

     public char convertGrades( int testResult) {
         char grade;
         if (testResult >= 90) {
             grade = 'A';
         } else if (testResult >= 80 && testResult < 90){
             grade = 'B';
         }else if (testResult >= 70 && testResult < 80){
             grade = 'C';
         }else {
             grade = 'D';
         }
         return grade;
     }

     public static void main(String[] args) {
         ReportCard rc = new ReportCard();
         rc.studentName = "Jerry Lee";

         char yourGrade = rc.convertGrades(60);
         switch (yourGrade){
             case 'A':
             System.out.println("Excellent Job!");
             break;
             case 'B':
             System.out.println("Good Job!");
             break;
             case 'D':
             System.out.println("Chage your attitude!");
             break;

         }
        }
    }
