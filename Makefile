CXX = g++
CXXFLAGS = -std=c++17 -Wall -O3

SRC_DIR = src
OBJ_DIR = obj

SRCS = $(SRC_DIR)/main.cpp \
       $(SRC_DIR)/cache.cpp \
       $(SRC_DIR)/processor.cpp \
       $(SRC_DIR)/bus.cpp \
       $(SRC_DIR)/simulate.cpp \
       $(SRC_DIR)/utils.cpp

OBJS = $(SRCS:$(SRC_DIR)/%.cpp=$(OBJ_DIR)/%.o)

TARGET = L1simulate

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp | $(OBJ_DIR)
	$(CXX) $(CXXFLAGS) -c -o $@ $<

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

clean:
	rm -rf $(OBJ_DIR) $(TARGET)

run: $(TARGET)
	./L1simulate -t app1 -s 7 -E 2 -b 5 -o results.txt

.PHONY: all clean run
