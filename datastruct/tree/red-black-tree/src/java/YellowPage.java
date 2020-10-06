/*
 * YellowPage.java
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
import java.io.*;

public class YellowPage {
    public static void main(String[] args) {
        Map<String, String> dict = new TreeMap<>();
        try {
            Scanner sc = new Scanner(new File("yp.txt"));
            while (sc.hasNext()) {
                String name = sc.next();
                String phone = sc.next();
                dict.put(name, phone);
            }
            sc = new Scanner(System.in);
            while (true) {
                System.out.print("name: ");
                String name = sc.next();
                String phone = dict.get(name);
                System.out.println(phone == null ? "not found" : ("phone: " + phone));
            }
        } catch (FileNotFoundException e) {
            System.out.println(e);
        }
    }
}
