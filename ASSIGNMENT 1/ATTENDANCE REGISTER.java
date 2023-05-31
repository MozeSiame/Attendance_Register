import java.util.Scanner;
public class Attenndance_Reg {
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        //Variable to store the lenght of the register array
        System.out.println("Enter the number of members in the attendance register: ");
        int x = input.nextInt();

        // Initiaise array and display prompt to enter the names
        String names[] = new String[x];
        System.out.println("Enter the names: ");

        
        // It gets the names from the console and input into the array
        for(int i = 0; i < names.length; i++){
        Scanner input2 = new Scanner(System.in);
        names[i] = input2.nextLine();
        }
        /*  prints the names of the array 
        System.out.println("The following are the names in the register: ");
          for(int i = 0; i < x; i++){
           System.out.println(names[i]);

        } */

        String[] presentArray = new String[names.length];
        String[] absentArray = new String[names.length];
        int presentIndex = 0;
        int absentIndex = 0;

        // Prompt the user to classify each element as absent or present
        for (int i = 0; i < names.length; i++) {
            System.out.print("Is " + names[i] + " present? (Y/N): ");
            String response = input.nextLine().toUpperCase();
            System.out.println();
            if (response.equals("Y")) {
                presentArray[presentIndex] = names[i];
                presentIndex++;
            } else if (response.equals("N")) {
                absentArray[absentIndex] = names[i];
                absentIndex++;
            } else {
                System.out.println("Invalid response. Please enter Y or N.");
                i--;
            }
        }
           // Print the present array
        System.out.println("Present Members: "+ " "+presentIndex);
        for (int i = 0; i < presentIndex; i++) {
            System.out.println(presentArray[i] + " ");
        }
        System.out.println();

        // Print the absent array
        System.out.println("Absent Members: "+ " "+absentIndex);
        for (int i = 0; i < absentIndex; i++) {
            System.out.println(absentArray[i] + " ");
        }
        System.out.println();

       
    }

    }

