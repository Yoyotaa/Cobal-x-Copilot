#!/bin/bash

# Automated Test Runner for Python Accounting Application
# This script runs all tests and generates comprehensive reports

echo "=================================="
echo "Python Accounting App Test Runner"
echo "=================================="

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Check if we're in the right directory
if [[ ! -f "main.py" || ! -f "operations.py" || ! -f "data.py" ]]; then
    echo -e "${RED}Error: Please run this script from the python-accounting-app directory${NC}"
    exit 1
fi

echo -e "${BLUE}Installing test dependencies...${NC}"
pip3 install -r requirements_test.txt

echo ""
echo -e "${BLUE}Running unittest-based automated tests...${NC}"
echo "----------------------------------------------"
python3 test_suite_automated.py

echo ""
echo -e "${BLUE}Running pytest-based tests with detailed reporting...${NC}"
echo "----------------------------------------------------"

# Run pytest with various output formats
pytest test_accounting_pytest.py -v \
    --html=test_report.html \
    --self-contained-html \
    --cov=operations \
    --cov=data \
    --cov=main \
    --cov-report=html:htmlcov \
    --cov-report=term-missing \
    --tb=short

echo ""
echo -e "${BLUE}Running tests by category...${NC}"
echo "----------------------------"

echo -e "${YELLOW}Running View Balance Tests:${NC}"
pytest test_accounting_pytest.py -v -m "view_balance"

echo -e "${YELLOW}Running Credit Tests:${NC}"
pytest test_accounting_pytest.py -v -m "credit"

echo -e "${YELLOW}Running Debit Tests:${NC}"
pytest test_accounting_pytest.py -v -m "debit"

echo -e "${YELLOW}Running Integration Tests:${NC}"
pytest test_accounting_pytest.py -v -m "integration"

echo ""
echo -e "${GREEN}Test execution completed!${NC}"
echo "=================================="
echo "Generated Reports:"
echo "- test_report.md (unittest detailed report)"
echo "- pytest_test_report.md (pytest detailed report)"
echo "- test_report.html (HTML test report)"
echo "- htmlcov/index.html (HTML coverage report)"
echo ""
echo -e "${BLUE}To view HTML reports:${NC}"
echo "- Test Report: open test_report.html in your browser"
echo "- Coverage Report: open htmlcov/index.html in your browser"
echo ""
echo -e "${BLUE}Test Summary Files:${NC}"
ls -la *.md *.html 2>/dev/null
echo ""

# Display quick summary
if [[ -f "test_report.md" ]]; then
    echo -e "${YELLOW}Quick Test Summary:${NC}"
    grep -E "Total Tests|Passed|Failed|Success Rate" test_report.md | head -4
fi
