package Utilities;

public class Sort {

    public static <T extends Comparable<T>> void sort(T[] array) {
        sort(array, 0, array.length - 1);
    }

    private static <T extends Comparable<T>> void sort(T[] array, int low, int high) {
        if (low >= high) {
            return;
        }

        int partitionIndex = partition(array, low, high);
        sort(array, low, partitionIndex - 1);
        sort(array, partitionIndex + 1, high);
    }

    private static <T extends Comparable<T>> int partition(T[] array, int low, int high) {
        T pivot = array[high];
        int i = (low - 1);

        for (int j = low; j <= high - 1; j++) {
            // array[j] < pivot
            if (array[j].compareTo(pivot) < 0) {
                i++;
                swap(array, i, j);
            }
        }

        swap(array, i + 1, high);
        return i + 1;
    }

    private static <T extends Comparable<T>> void swap(T[] array, int i, int j) {
        T n = array[i];
        array[i] = array[j];
        array[j] = n;
    }
}

