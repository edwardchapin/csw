package javacsw.util.config;

import csw.util.config.*;
import csw.util.config.Configurations.SetupConfig;
import org.junit.Test;

import java.util.Objects;
import java.util.Optional;

/*
class MyArrayKey extends KeyTypeArray<Integer> {
  private MyArrayKey(Vector<Integer> data) {

    super("value", data, JUnitsOfMeasure.Meters);
  }

  static MyArrayKey adata(int... values) {
    Vector<Integer> data = new Vector(Arrays.asList(values));

    return new MyArrayKey(data);
  }
}
*/

// An example of defining a key in Java
class JFilter extends Key1<String> {
    JFilter() {
        super("filter", JUnitsOfMeasure.NoUnits);
    }

    public CItem<String> set(String v) {
        return new CItem<>(this, units(), v);
    }

    static CItem<String> filter(String value) {
        return (new JFilter()).set(value);
    }
}

/**
 * Tests the Java API to the config classes
 */
@SuppressWarnings("OptionalGetWithoutIsPresent")
public class JConfigKeyTests {
    private static final String ck = "wfos.blue.filter";
    private static final String ck1 = "wfos.prog.cloudcover";
    private static final String ck2 = "wfos.red.filter";
    private static final String ck3 = "wfos.red.detector";

    private static final String s1 = "encoder";
    private static final String s2 = "filter";
    private static final String s3 = "detectorTemp";

    @Test
    public void testKey() {
        IntKey k1 = new IntKey(s1, JUnitsOfMeasure.NoUnits);
        StringKey k2 = new StringKey(s2, JUnitsOfMeasure.Meters);

        assert (Objects.equals(k1.name(), s1));
        assert (k1.units() == JUnitsOfMeasure.NoUnits);

        Item<Integer> i = k1.set(22);
        assert (Objects.equals(i.key().name(), s1));
        assert (i.key().units() == JUnitsOfMeasure.NoUnits);
        assert (i.value() == 22);

        assert (Objects.equals(k2.name(), s2));
        assert (k2.units() == JUnitsOfMeasure.Meters);
        Item<String> j = k2.set("Bob");
        assert (Objects.equals(j.value(), "Bob"));

        IntKey k3 = new IntKey(s1, JUnitsOfMeasure.NoUnits);
        assert (k3.equals(k1));
    }

    @Test
    public void arrayKey() {
//        IntArrayKey ia = new IntArrayKey("iarray", JUnitsOfMeasure.Deg);
        ArrayKey<Integer> ia = new ArrayKey<>("iarray", JUnitsOfMeasure.Deg);
        // TODO: FIXME
//        Item ci = ia.jset(1, 2, 3);
        Item ci = ia.jset(1, 2, 3);
    }

    @Test
    public void jKeyTest() {
        JKey1<String> k1 = new JKey1<>(s2, JUnitsOfMeasure.NoUnits);
        Item<String> i = k1.set("blue");
        assert (Objects.equals(k1.name(), s2));
        assert (k1.units() == JUnitsOfMeasure.NoUnits);
        assert (Objects.equals(i.value(), "blue"));


        JKey1<Double> k2 = new JKey1<>(s3, JUnitsOfMeasure.Deg);
        assert (Objects.equals(k2.name(), s3));
        assert (k2.units() == JUnitsOfMeasure.Deg);
        Item<Double> j = k2.set(34.34);
        assert (j.value() == 34.34);
    }

    @Test
    public void testConfig() {
        IntKey k1 = new IntKey(s1, JUnitsOfMeasure.NoUnits);
        Item<Integer> i1 = k1.set(22);

        StringKey k2 = new StringKey(s2, JUnitsOfMeasure.Meters);
        Item<String> i2 = k2.set(s2);

        SetupConfig sc = new SetupConfig(ck2).add(i1).add(i2);
        assert (sc.size() == 2);

        Optional<Item<Integer>> ki1 = sc.jget(k1);
        assert (ki1.get() == i1);
        assert (22 == ki1.get().value());

        Optional<Item<String>> ki2 = sc.jget(k2);
        assert (ki2.get() == i2);
        assert (Objects.equals(s2, ki2.get().value()));

        // Check setting through sc
        SetupConfig sc2 = new SetupConfig(ck3);
        sc2 = sc2.add(k1.set(22));
        sc2 = sc2.add(k2.set(s2));
        System.out.println("SC2: " + sc2);
        System.out.println("SC2: " + i1);
        System.out.println("SC2: " + sc2.get(k1).get());


        assert (sc2.jget(k1).get().equals(i1));
        assert(22 == sc2.jget(k1).get().value());
        assert(ki2.get() == i2);
        assert(Objects.equals(s2, ki2.get().value()));

        System.out.println("SC2: " + sc2);

    }


    @Test
    public void customJavaKey() {
        // Create an instance
        JFilter filter = new JFilter();
        assert (Objects.equals(filter.name(), s2));
        assert (filter.units() == JUnitsOfMeasure.NoUnits);

        Item<String> i = filter.set("blue");
        assert (Objects.equals(i.value(), "blue"));

        SetupConfig sc = new SetupConfig(ck1).add(i);
        System.out.println("SC: " + sc);

        SetupConfig sc2 = new SetupConfig(ck1).set(filter, "red");
        System.out.println("SC2: " + sc2);

        SetupConfig sc3 = new SetupConfig(ck1).add(JFilter.filter("green")).add(JFilter.filter("blue"));
        System.out.println("SC3: " + sc3);

    }

    @Test
    public void testRemove() {
        IntKey k1 = new IntKey(s1, JUnitsOfMeasure.NoUnits);
        StringKey k2 = new StringKey(s2, JUnitsOfMeasure.Meters);
        DoubleKey k3 = new DoubleKey(s3, JUnitsOfMeasure.Deg);

        Item<Integer> i1 = k1.set(22);
        Item<String> i2 = k2.set(s2);
        Item<Double> i3 = k3.set(-34.56);

        SetupConfig sc = new SetupConfig(ck2).add(i1).add(i2).add(i3);
        assert (sc.size() == 3);

        sc = sc.remove(k1);
        assert (sc.size() == 2);
        assert (!sc.exists(k1));
        assert (sc.exists(k2));
        assert (sc.exists(k3));
        sc = sc.remove(k2);
        assert (sc.size() == 1);
        assert (!sc.exists(k1));
        assert (!sc.exists(k2));
        assert (sc.exists(k3));
        sc = sc.remove(k3);
        assert (sc.size() == 0);
        assert (!sc.exists(k1));
        assert (!sc.exists(k2));
        assert (!sc.exists(k3));
    }
}
