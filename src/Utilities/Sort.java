package Utilities;

import java.util.List;

public class Sort {

    public static <T extends Comparable<T>> void sort(List<T> array) {
        sort(array, 0, array.size() - 1);
    }

    private static <T extends Comparable<T>> void sort(List<T> array, int low, int high) {
        if (low >= high) {
            return;
        }

        int partitionIndex = partition(array, low, high);
        sort(array, low, partitionIndex - 1);
        sort(array, partitionIndex + 1, high);
    }

    private static <T extends Comparable<T>> int partition(List<T> array, int low, int high) {
        T pivot = array.get(high);
        int i = (low - 1);

        for (int j = low; j <= high - 1; j++) {
            // array[j] < pivot
            if (array.get(j).compareTo(pivot) < 0) {
                i++;
                swap(array, i, j);
            }
        }

        swap(array, i + 1, high);
        return i + 1;
    }

    private static <T extends Comparable<T>> void swap(List<T> array, int i, int j) {
        T n = array.get(i);
        array.set(i, array.get(j));
        array.set(j, n);
    }
}

