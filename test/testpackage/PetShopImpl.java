package testpackage;

import java.util.ArrayList;
import java.util.List;

class Hamster implements Animal {

    @Override
    public boolean isFurry() {
        return true;
    }
}

class Molerat implements Animal {

    @Override
    public boolean isFurry() {
        return false;
    }
}

public class PetShopImpl implements PetShop {

    public PetShopImpl() {}

    public String toString() { return "a pet shop"; }

    @Override
    public List<Pet> getPetsForSale() {
        List<Pet> res = new ArrayList<Pet>();

        res.add(new Pet() {

            @Override
            public String getName() {
                return "Harry";
            }

            @Override
            public Animal getKind() {
                return new Hamster();
            }

            @Override
            public int getAge() {
                return 3;
            }

            @Override
            public Personality getPersonality() {
                return Personality.HAPPY;
            }
        });

        res.add(new Pet() {

            @Override
            public String getName() {
                return "Mike";
            }

            @Override
            public Animal getKind() {
                return new Molerat();
            }

            @Override
            public int getAge() {
                return 4;
            }

            @Override
            public Personality getPersonality() {
                return Personality.STOIC;
            }
        });

        return res;
    }

    //@Override
    public Pet[] getPetsForSale1() {
        Pet[] arr = new Pet[getPetsForSale().size()];
        return getPetsForSale().toArray(arr);
    }
}

