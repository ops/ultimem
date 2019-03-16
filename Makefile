CFLAGS = -O3 -Wall -pedantic

TARGET = makecart
TARGET_FLASH = flash4file
TARGET_FLASH_SRCS = flash4file.c
PRGS = ultimem.prg ramtest.prg flash4file.prg banktest.prg
TARGETS = $(TARGET) $(TARGET_FLASH) $(CBM)

CBM = flash512k.prg flash8m.prg $(PRGS)
SRCS = makecart.c readline.c
OBJS = $(SRCS:.c=.o)
TARGET_FLASH_OBJS = $(TARGET_FLASH_SRCS:.c=.o)
ALLOBJS = $(OBJS) $(TARGET_FLASH_OBJS)

all: $(TARGETS)
clean:
	rm -f $(ALLOBJS)
reallyclean: clean
	rm -f $(TARGETS)

.phony: all clean reallyclean depend
.SUFFIXES: .s .prg .bin .img

$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS)
$(TARGET_FLASH): $(TARGET_FLASH_OBJS)
	$(CC) $(LDFLAGS) -o $@ $(TARGET_FLASH_OBJS)

main.o: readline.h
readline.o: readline.h

ultimem512k-s.img: menu.img $(PRGS) prg.txt $(TARGET)
	./$(TARGET) $< $@ 524288 < prg.txt
ultimem8m-s.img: menu.img $(PRGS) prg.txt $(TARGET)
	./$(TARGET) $< $@ 8388608 < prg.txt

.img.bin: $(TARGET_FLASH)
	./$(TARGET_FLASH) $< $@
.s.prg:
	xa -o $@ $<
flash4file.prg: flash.s
	xa -DFILEIO=1 -o $@ $<
flash8m.prg: flash.s ultimem8m-s.bin
	xa -DFILEIO=0 -o $@ $<
	cat ultimem8m-s.bin >> $@
flash512k.prg: flash.s ultimem512k-s.bin
	xa -DFILEIO=0 -o $@ $<
	cat ultimem512k-s.bin >> $@
menu.img: menu.s
	xa -o $@ $<

# You can adapt these rules yourself, for your own content. Try:
# make ultimem8m.bin
# xvic -ultimem ultimem8m.img
ultimem512k.img: menu.img filelist.txt $(PRGS) $(TARGET)
	./$(TARGET) $< $@ 524288 < filelist-small.txt
ultimem8m.img: menu.img filelist.txt $(TARGET)
	./$(TARGET) $< $@ 8388608 < filelist.txt
