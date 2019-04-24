filename<-paste0(directoryname,"/Data/PEAggData.csv")

lag.name<-c("logprice","logsales")
het.name<-paste0("Level",as.character(2:5))
key.name<-c("SiteName","ChannelName","Item")
seed<-888

# Names of categories and codes


xbreaks<-list()

# numeric order corresponds to categorical order
# 188,166,0
xbreaks[[1]]<-c("Water","Sodas","Alcohol")
# 12,5,19,8
xbreaks[[2]]<-c("Meat","Dairy","Chicken","Seafood")
# 15,11,4,26
xbreaks[[3]]<-c("Boxes","Sanitation","Tableware",  "Stationery")
# 25,6,28,27
xbreaks[[4]]<-c("Snacks","Sweets","Veggies","Sugar")

outname_list<-list()
outname_list[[1]]<-"Drinks"
outname_list[[2]]<-"Dairy"
outname_list[[3]]<-"NonEdible"
outname_list[[4]]<-"Snacks"

if (length(level1_inds)>1){
  het.name<-c(het.name,"Level1")
}
treat.name<-"logpr"
outcome.name<-"logmove"
library(gamlr)
method.treat<-cv.gamlr
method.outcome<-cv.gamlr
num_splits<-2
outfile<-paste0(directoryname,"/JJFoods/Output/FirstStage",as.character(level1_inds)[1],".csv")

figdirectory<-paste0(directoryname,"/JJFoods/Output/Figures/")
textdirectory<-paste0(directoryname,"/JJFoods/Output/Text/")
outtexname<-paste0(textdirectory,"Own",".tex")


## Names for cross-price matrices of products
product_list<-list()

product_list[[1]]<-c("Still Water in Plastic","Still Water in Glass","Spark Water in Plastic","Spark Water in Glass",
                 "Spark Water in Cans","Smoothies","Rose Wine","Red Wine","LL Juice Drinks","LL Fruit Juice",
                 "Lemonade","Ice Tea","Flvrd Fizzy Drinks","Cola","Chilled Fruit Juice","Champagne",
                 "Spirits","Other Wine","Vitamin Water","Energy",
                 "Milkshake","Other Water","Other Soft Drinks","Other Drinks")

product_list[[2]]<-c("Cod","Chicken Wing","Chicken Thigh","Chicken Strips",
                     "Chicken Steak","Chicken Nugget","Chicken Leg",
                     "Chicken Kiev","Chicken Fillet","Chicken Drumstick",
                     "Block Cheddar","Black Tiger","Black Pudding", "Battered Fish",
                     "Whole Chicken","Fresh Pork","Fish Fingers","Fish Cakes",
                     "Cheddar","Canned Cod","Canned Anchoives","Whole Milk",
                     "White Cheese","Turkey Pizza","Burger Cheese","Steak",
                     "Soya Milk","Soft Cheese","Skimmed Milk","Semi Skimmed Milk",
                     "Seafood Mix","Scampi","Scallops","Raw Prawns","Raw Fish","Processed Eggs","Prepared Fish","Prawns,Other","Pork Pizza Topping","Pepperoni Pizza Cheese","Beef Pizza Toppings",
                     "Mussels","Mozzarella Cheese","Mozzarella & Cheddar","Mozzarella & Analouge",
                     "Mozzarella","Halloumi Cheese","Frozen Pork","Frozen Lamb","Frozen Beef",
                     "Lamb & Mutton, Other","Beef, Other","Ghee","Frozen Raw Seafood","Frozen Prawn",
                     "Frozen Fish","Frozen Coated Seafood","Eggs","Dairy Spread","Cream",
                     "Chicken Specialities","Chicken","Cheese, Other","Canned Fish",
                     "Yoghurt","Butter","Turkey","Pork, Other","Pizza Toppings","Pizza Cheese",
                     "Milk Powder","Milk","Meat Speciality","Margarine","Fish, Other","Dairy, Other","Poultry,Other")

product_list[[3]]<-c("White Grease Resistant Bags",
                     "White Carrier Bags",
                     "Tortilla Wrap Cover",
                     "Printed Takeaway Bags",
                     "Kebab Wrap",
                     "Calzone Boxes",
                     "Burger Wrap",
                     "Brown Takeaway Bags",
                     "Brown SOS Bags",
                     "Brown Printed Bags",
                     "Brown Pizza Boxes",
                     "Brown Carrier Bags",
                     "White Vest Carrier Bags",
                     "White Unstrung Bags",
                     "White Strung Bags",
                     "White SOS Bags",
                     "White Printed Bags",
                     "White Pizza Boxes",
                     "White GreaseProof Bags",
                     "Foil Container",
                     "Foil",
                     "Disposable Gloves",
                     "Dishwasher Salt",
                     "Dishwasher Liquid",
                     "Cling Film",
                     "Chip Bags",
                    "Chef Hats Aprons",
                    "Centerfeed Rolls",
                    "White Bags",
                    "Washing Up Liquids",
                    "Vest Carrier Bags",
                    "Toilet Rolls",
                    "Bin Liners",
                    "Serviettes",
                    "Scouers and Pads",
                    "Sandwich Bags",
                    "Box Liners",
                    "Polystyrene Trays",
                    "Polystyrene Meal Boxes",
                    "Polystyrene  Containers",
                    "Polystyrene Container Lids",
                    "Bleach","Polystyrene Boxes",
                    "Plastic Cutlery",
                    "Plastic Containers",
                    "Pizza Tripods",
                    "Pizza Boxes",
                    "Pizza Box Liners",
                    "Oven Cleaners",
                    "Gloves",
                    "Laundry Powders",
                    "Laundry Liquids",
                    "Kitchen Towels",
                    "Kitchen Degreasers",
                    "Kitchen Cleaners",
                    "Janitorial",
                    "Hot Chicken Boxes",
                    "Heavy Duty Bin Liners",
                    "Heavy Duty Rubber Gloves",
                    "Hand Towels",
                    "Greasproof Paper",
                    "Other Laundry Products",
                    "Other Kitchen and Glass Cleaning",
                    "Hygiene Ancillarries",
                    "Hand Soaps",
                    "Other Gloves",
                    "Other Cleaning",
                    "Other Food Containers",
                    "Floorcare",
                    "Dishwashing",
                    "Other cutlery",
                    "Cleaning & Hygiene Supplies",
                    "Other Bags","Chicken Boxes",
                    "Cardboard Fish and Chips Boxes",
                    "Wrapping Materials",
                    "Toileteries & Washroom",
                    "Polystyrene Packaging, Other",
                    "Pizza Packaging, Other",
                    "Paper Hygiene, Other",
                    "Crockery, Other",
                    "Stationery, Other",
                    "Packing, Other", "Hygiene, Other")


product_list[[4]]<-c("Cheesecake","Thick Shake Mix","Sliced Cakes","Loaf Cakes",
                 "Ice Cream Cone","Ice Cream","Fruit Cake","Lollypop",
                 "Ice Cream Syrup","Gateau","Bars","Dessert Spec","Demerara Sugar",
                 "Custard","Crisps","Chocolate","Gum","Cakes",
                 "Waffle","Wafers","Veggie Burgers","Sweeteners","Sugar Sticks","Small Dessers","Quorn Meals","Prawn Crackers",
                 "Poppadoms","Popcorn","Pie","Party Snacks","Onion Rings",
                 "Nuts and Dried Fruit","Cookies","Dessert","Sugar","Spread")

