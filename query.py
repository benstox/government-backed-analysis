import numpy
import pandas

from django.utils import timezone
from fdbapp.models import Fund, Fundraising, InvestmentType, RegionGroup, Sector

year_start = timezone.datetime.strptime("2011-01-01", "%Y-%m-%d")
year_end = timezone.datetime.strptime("2011-12-31", "%Y-%m-%d")

# ?investee__company__country__id__exact=gb&primary_date__gte=2011-01-01&primary_date__lte=2011-12-31&published__exact=1&investment_type__id=7
fs = Fundraising.objects.filter(
    investee__company__country__id__exact="gb",
    primary_date__gte=year_start, primary_date__lte=year_end,
    investment_type__id=7, published=True)

regions = RegionGroup.objects.all()

with open("regions.csv", "w") as f:
    f.write("region,local_authority\n")
    for region in regions:
        for member in region.members.all():
            f.write("{},{}\n".format(region.name, member.name))

sectors = Sector.objects.all()

with open("sectors.csv", "w") as f:
    f.write("sector_group|sector\n")
    for sector in sectors:
        if sector.primary_group.parent:
            f.write("{}|{}\n".format(sector.primary_group.parent.name, sector.name))
        else:
            f.write("{}|{}\n".format(sector.primary_group.name, sector.name))
