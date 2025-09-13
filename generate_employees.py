# imports
import random
import csv
from faker import Faker

fake = Faker()

num_employees = 100
output_file = "employees.csv"

with open(output_file, mode="w", newline="") as file:
    writer = csv.writer(file)
    for emp_id in range(1, num_employees + 1):
        name = fake.name()
        hours = random.randint(30, 45) # typical work hours
        rate = round(random.uniform(10, 50), 2) # hourly rate between 10 and 50
        writer.writerow([f"{emp_id:04d}", name, hours, f"{rate:.2f}"])

print(f"{num_employees} demo employees written to {output_file}")

