# Use Python 3.10 slim image for production
FROM python:3.10-slim

# Set working directory
WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    gcc \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user for security
RUN groupadd -r appuser && useradd -r -g appuser appuser

# Copy requirements and install Python dependencies
COPY requirements.txt .
COPY python-accounting-app/requirements_test.txt ./test-requirements.txt

RUN pip install --no-cache-dir -r requirements.txt

# Copy application code
COPY python-accounting-app/ ./

# Create data directory with proper permissions
RUN mkdir -p /app/data && chown -R appuser:appuser /app

# Switch to non-root user
USER appuser

# Set environment variables
ENV PYTHONPATH=/app
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1
ENV DATA_FILE=/app/data/account_balance.json

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD python -c "import operations; ops = operations.Operations(); print('Health check passed')" || exit 1

# Expose port (if web interface is added later)
EXPOSE 8000

# Default command
CMD ["python", "main.py"]
