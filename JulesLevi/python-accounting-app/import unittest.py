import unittest
from unittest.mock import Mock, patch, MagicMock
import sys
import os
from operations import Operations


class TestOperations(unittest.TestCase):
    """Test class for Operations functionality."""
    
    def setUp(self):
        """Set up test fixtures before each test method."""
        self.operations = Operations()
        self.operations.data_program = Mock()
    
        self.assertEqual(result, expected_new_balance)
        self.operations.data_program.perform_operation.assert_any_call('READ')
        self.operations.data_program.perform_operation.assert_any_call('WRITE', expected_new_balance)
        mock_print.assert_called_once_with("Amount credited. New balance: 150.25")
    
    @patch('builtins.input', side_effect=['-10', '0', '25.50'])
    def test_credit_account_invalid_amounts(self, mock_input):
        """Test credit_account handles invalid amounts."""
        # Arrange
        current_balance = 100.00
        expected_new_balance = 125.50
        self.operations.data_program.perform_operation.side_effect = [current_balance, None]
        
        # Act
        with patch('builtins.print') as mock_print:
            result = self.operations.credit_account()
        
        # Assert
        self.assertEqual(result, expected_new_balance)
        self.assertEqual(mock_input.call_count, 3)
    
    @patch('builtins.input', side_effect=['invalid', 'abc', '30.00'])
    def test_credit_account_invalid_input_format(self, mock_input):
        """Test credit_account handles non-numeric input."""
        # Arrange
        current_balance = 100.00
        expected_new_balance = 130.00
        self.operations.data_program.perform_operation.side_effect = [current_balance, None]
        
        # Act
        with patch('builtins.print') as mock_print:
            result = self.operations.credit_account()
        
        # Assert
        self.assertEqual(result, expected_new_balance)
        self.assertEqual(mock_input.call_count, 3)
    
    def test_credit_account_exception(self):
        """Test credit_account handles exceptions."""
        # Arrange
        self.operations.data_program.perform_operation.side_effect = Exception("Network error")
        
        # Act
        with patch('builtins.input', return_value='50'):
            with patch('builtins.print') as mock_print:
                result = self.operations.credit_account()
        
        # Assert
        self.assertIsNone(result)
        mock_print.assert_called_with("Error processing credit: Network error")
    
    @patch('builtins.input', return_value='30.00')
    def test_debit_account_success(self, mock_input):
        """Test debit_account successfully debits amount."""
        # Arrange
        current_balance = 100.00
        expected_new_balance = 70.00
        self.operations.data_program.perform_operation.side_effect = [current_balance, None]
        
        # Act
        with patch('builtins.print') as mock_print:
            result = self.operations.debit_account()
        
        # Assert
        self.assertEqual(result, expected_new_balance)
        self.operations.data_program.perform_operation.assert_any_call('READ')
        self.operations.data_program.perform_operation.assert_any_call('WRITE', expected_new_balance)
        mock_print.assert_called_once_with("Amount debited. New balance: 70.00")
    
    @patch('builtins.input', return_value='150.00')
    def test_debit_account_insufficient_funds(self, mock_input):
        """Test debit_account handles insufficient funds."""
        # Arrange
        current_balance = 100.00
        self.operations.data_program.perform_operation.return_value = current_balance
        
        # Act
        with patch('builtins.print') as mock_print:
            result = self.operations.debit_account()
        
        # Assert
        self.assertEqual(result, current_balance)
        self.operations.data_program.perform_operation.assert_called_once_with('READ')
        mock_print.assert_called_once_with("Insufficient funds for this debit.")
    
    @patch('builtins.input', side_effect=['-20', '0', '40.00'])
    def test_debit_account_invalid_amounts(self, mock_input):
        """Test debit_account handles invalid amounts."""
        # Arrange
        current_balance = 100.00
        expected_new_balance = 60.00
        self.operations.data_program.perform_operation.side_effect = [current_balance, None]
        
        # Act
        with patch('builtins.print') as mock_print:
            result = self.operations.debit_account()
        
        # Assert
        self.assertEqual(result, expected_new_balance)
        self.assertEqual(mock_input.call_count, 3)
    
    def test_debit_account_exception(self):
        """Test debit_account handles exceptions."""
        # Arrange
        self.operations.data_program.perform_operation.side_effect = Exception("File error")
        
        # Act
        with patch('builtins.input', return_value='25'):
            with patch('builtins.print') as mock_print:
                result = self.operations.debit_account()
        
        # Assert
        self.assertIsNone(result)
        mock_print.assert_called_with("Error processing debit: File error")
    
    def test_perform_operation_total(self):
        """Test perform_operation with TOTAL operation."""
        # Arrange
        expected_balance = 200.00
        self.operations.data_program.perform_operation.return_value = expected_balance
        
        # Act
        with patch('builtins.print'):
            result = self.operations.perform_operation('TOTAL')
        
        # Assert
        self.assertEqual(result, expected_balance)
    
    @patch('builtins.input', return_value='75.00')
    def test_perform_operation_credit(self, mock_input):
        """Test perform_operation with CREDIT operation."""
        # Arrange
        current_balance = 100.00
        expected_new_balance = 175.00
        self.operations.data_program.perform_operation.side_effect = [current_balance, None]
        
        # Act
        with patch('builtins.print'):
            result = self.operations.perform_operation('CREDIT')
        
        # Assert
        self.assertEqual(result, expected_new_balance)
    
    @patch('builtins.input', return_value='25.00')
    def test_perform_operation_debit(self, mock_input):
        """Test perform_operation with DEBIT operation."""
        # Arrange
        current_balance = 100.00
        expected_new_balance = 75.00
        self.operations.data_program.perform_operation.side_effect = [current_balance, None]
        
        # Act
        with patch('builtins.print'):
            result = self.operations.perform_operation('DEBIT')
        
        # Assert
        self.assertEqual(result, expected_new_balance)
    
    def test_perform_operation_invalid_type(self):
        """Test perform_operation with invalid operation type."""
        # Act
        with patch('builtins.print') as mock_print:
            result = self.operations.perform_operation('INVALID')
        
        # Assert
        self.assertIsNone(result)
        mock_print.assert_called_once_with("Unknown operation type: INVALID")
    
    def test_perform_operation_case_insensitive(self):
        """Test perform_operation handles case insensitive input."""
        # Arrange
        expected_balance = 150.00
        self.operations.data_program.perform_operation.return_value = expected_balance
        
        # Act
        with patch('builtins.print'):
            result = self.operations.perform_operation('  total  ')
        
        # Assert
        self.assertEqual(result, expected_balance)

if __name__ == '__main__':
    unittest.main()