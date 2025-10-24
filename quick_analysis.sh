#!/bin/bash

echo "====== EARTHQUAKE DATA QUICK ANALYSIS ======"
echo ""
echo "Total earthquakes: $(tail -n +2 all_month.csv | wc -l)"
echo ""
echo "Top 10 largest earthquakes:"
tail -n +2 all_month.csv | cut -d',' -f5,21 | grep -v '^,' | sort -t',' -k1 -nr | head -10
echo ""
echo "Magnitude statistics (approx):"
echo "  Largest: $(tail -n +2 all_month.csv | cut -d',' -f5 | grep -v '^$' | sort -n | tail -1)"
echo "  Smallest: $(tail -n +2 all_month.csv | cut -d',' -f5 | grep -v '^$' | sort -n | head -1)"
echo ""
echo "Count by magnitude range:"
echo "  >= 5.0: $(tail -n +2 all_month.csv | cut -d',' -f5 | awk '$1 >= 5.0' | wc -l)"
echo "  >= 4.0: $(tail -n +2 all_month.csv | cut -d',' -f5 | awk '$1 >= 4.0' | wc -l)"
echo "  >= 3.0: $(tail -n +2 all_month.csv | cut -d',' -f5 | awk '$1 >= 3.0' | wc -l)"
