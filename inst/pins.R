# Read raw data, parse, and add as pin to rsconnect
# 2 pins: one for private use, one for public use

file_path = system.file("app/fake_data/tabs_3_and_4.csv",
                        package = "retentionsApp",
                        mustWork = TRUE)

raw_retention = read_csv(file_path)

board_rsc = pins::board_rsconnect(auth = "rsconnect")
pins::pin_write(board_rsc, raw_retention, name = "private_retention")

# Remove any columns containing sensitive data
parsed_retention = dplyr::select(raw_retention, -student_id)

pins::pin_write(board_rsc, parsed_retention, name = "public_retention")
