# --- Build image ---
FROM mcr.microsoft.com/dotnet/sdk:6.0 as build

# Install node
RUN curl -sL https://deb.nodesource.com/setup_16.x | bash
RUN apt-get update && apt-get install -y nodejs

# Copy all sources
WORKDIR /workspace
COPY . .

# Install tools and compile
RUN dotnet tool restore
RUN dotnet run Bundle

# --- Run image ---
FROM mcr.microsoft.com/dotnet/aspnet:6.0-alpine

# Configure default storage location
RUN mkdir /data
VOLUME /data
ENV HELPEENERROR_Database__Filepath=/data/data.db

# Expose the HTTP port
EXPOSE 8085

# Copy binaries
COPY --from=build /workspace/deploy /app

# Move to app directory and start server
WORKDIR /app
ENTRYPOINT [ "dotnet", "Server.dll" ]
