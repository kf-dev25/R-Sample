library(data.table)
library(dplyr)

# ���s�̍ۂ�utstein_data�̃C���|�[�g���K�v

# �����f�[�^�̃G���[�r���Ǝ����f�[�^�̕ϊ�####
cleansed_utstein_data <-
  utstein_data %>%
  filter(�N %in% 2005:2016) %>%
  filter(�N >= 2010) %>%
  filter(�N�� %in% 0:115) %>%
  # 18�Έȉ��͈�w�I�ɏ������҂ƌ��Ȃ���A���l�ƕa�Ԃ⏈�u���@�����قȂ��Ă��邽��
  filter(�N�� >= 18) %>%
  filter(�~�}�~���m��� == 1) %>%
  filter(��t�̏�� == 2) %>%
  filter(�S�g�@�\�J�e�S���[ %in% 1:5) %>%
  filter(!is.na(�N��)) %>%
  # �g�`���2�͌��������Ȃ����邽�߁A�܂��g�`���5�́u���̑��v�ƂȂ��Ă����ʂ̓��肪�s�̂��ߏ��O
  filter(�g�`��� == 1 | �g�`��� == 3 | �g�`��� == 4) %>%
  filter(��ܓ��^ %in% 0:1) %>%
  filter(����s�׊��g�p %in% 0:1) %>%
  filter(����s�׊���� %in% 0:3) %>%
  filter((����s�׊��g�p == 0 & ����s�׊���� == 0) | (����s�׊��g�p == 1 & ����s�׊���� %in% 0:3)) %>%
  mutate(��S�����̎�� = ifelse(is.na(��S�����̎��), 0, ��S�����̎��)) %>%
  filter(��S�����̎�� %in% 0:5)%>%
  mutate(�S���ĊJ�̗L�� = if_else(�S���ĊJ == 1, TRUE, FALSE)) %>%
  mutate(�����̗L�� = if_else(�S�g�@�\�J�e�S���[ >= 1 & �S�g�@�\�J�e�S���[ <= 4, TRUE, FALSE)) %>%
  mutate(�@�\�\�� = if_else(�S�g�@�\�J�e�S���[ >= 1 & �S�g�@�\�J�e�S���[ <= 2, TRUE, FALSE)) %>%
  
  # ver-4�n��R�ł͎��n��f�[�^���ŏ�����POSIXct�Ƃ��ĔF������邽�ߕϊ��s�v
  # mutate(�����N���� = as.POSIXct(strptime(�����N����, "%Y/%m/%d %H:%M"))) %>%
  # mutate(�ڌ����� = as.POSIXct(strptime(�ڌ�����, "%Y/%m/%d %H:%M"))) %>%
  # mutate(B_CPR�J�n���� = as.POSIXct(strptime(B_CPR�J�n����, "%Y/%m/%d %H:%M"))) %>%
  # mutate(���񏜍ד����{���� = as.POSIXct(strptime(���񏜍ד����{����, "%Y/%m/%d %H:%M"))) %>%
  # mutate(��ܓ��^���� = as.POSIXct(strptime(��ܓ��^����, "%Y/%m/%d %H:%M"))) %>%
  # mutate(�o�m = as.POSIXct(strptime(�o�m, "%Y/%m/%d %H:%M"))) %>%
  # mutate(���� = as.POSIXct(strptime(����, "%Y/%m/%d %H:%M"))) %>%
  # mutate(�ڐG = as.POSIXct(strptime(�ڐG, "%Y/%m/%d %H:%M"))) %>%
  # mutate(CPR�J�n = as.POSIXct(strptime(CPR�J�n, "%Y/%m/%d %H:%M"))) %>%
  # mutate(�a�@���e = as.POSIXct(strptime(�a�@���e, "%Y/%m/%d %H:%M"))) %>%
  # mutate(����S���ĊJ���� = as.POSIXct(strptime(����S���ĊJ����, "%Y/%m/%d %H:%M"))) %>%

  # ���ԃf�[�^��ϊ�(�󔒂Ȃǂ�NA�ɂȂ�)
  mutate(�������� = as.numeric(difftime(����, �o�m, units = "mins"))) %>%
  mutate(�a�@���e���� = as.numeric(difftime(�a�@���e, ����, units = "mins"))) %>%
  filter(�������� >= 0, �a�@���e���� >= 0) %>%
  filter(�������� <= 1440, �a�@���e���� <= 1440) %>%
  filter(�C���m�� == 1)