/*
 * WordCount.java
 * Copyright (C) 2018 Liu Xinyu (liuxinyu95@gmail.com)
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
import java.util.*;

public class WordCount {
    public static void main(String[] args) {
        Map<String, Integer> dict = new TreeMap<>();
        Scanner sc = new Scanner(System.in);
        while (sc.hasNext()) {
            String key = sc.next();
            dict.put(key, 1 + (dict.containsKey(key) ? dict.get(key) : 0));
        }
        for (Map.Entry<String, Integer> e : dict.entrySet())
            System.out.format("%s: %d\n", e.getKey(), e.getValue());
    }
}
