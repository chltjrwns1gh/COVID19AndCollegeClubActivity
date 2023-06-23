import csv

f = open("C:\\Users\\bb\\Desktop\\COVID19AndCollegeClubActivity\\제목 없는 설문지.csv", encoding='utf-8')
reader = csv.reader(f)
data = []
for lin in reader:
    data.append(lin)
f.close()

# data[0] = ['타임스탬프'
            # , '작년 기준 학년을 선택해주세요'
            # , '태어난 년도를 선택해주세요.'
            # , '위 기간 2022년 4월 (동아리/동문회 신입생 선발 이후) ~ 2023년 2월(2023학년 개강 전) 동안 \n참여 한 동아리와 동문회는 몇 개인가요? '
            # , '위 기간  2022년 4월 (동아리/동문회 신입생 선발 이 후) ~ 2023년 2월(2023학년 개강 전)  동안 \n코로나 양성(확진)이 몇 번 되었습니까? (병원  신속검사 양성, PCR 모두 해당됨)'
            # , '2022년 4월 (동아리/동문회 신입생 선발 이후) ~ 2023년 2월(2023학년 개강 전)  \n참여한 동아리/동문회 활동을 모두 체크해주세요. '
            # , '개인정보 수집 및 이용 동의'
            # , '질문이 모두 끝났습니다. ']

# 일단, 타임스탬프 행[0], 개인정보 수집 및 이용 동의 행[6], 질문이 모두 끝났습니다 행[7] 제거하기
for i in range(len(data)):
    data[i] = data[i][1:6]

# 이하 필요없는 행 제거한 기준으로 [n]을 표기했음
# 작년 기준 학년[0] 축약하기
# e.g. '작년에 본2' -> '4'
# e.g. '작년에 예1' -> '1'
for i in range(len(data)):
    if data[i][0] == '작년에 예1':
        data[i][0] = '1'
    elif data[i][0] == '작년에 예2':
        data[i][0] = '2'
    elif data[i][0] == '작년에 본1':
        data[i][0] = '3'
    elif data[i][0] == '작년에 본2':
        data[i][0] = '4'
    # else는 설문지의 구조상 불가능함.

# 태어난 년도[1] 그대로 둘 것

# 참여한 동아리/동문회 개수[2] 중, '5개 이상' 항목을 수정하기
# '5개 이상' -> '5'
for i in range(len(data)):
    if data[i][2] == '5개 이상':
        data[i][2] = '5'

# 코로나 확진 횟수[3] 중 '4번 이상' 항목을 수정하기
# '4번 이상' -> '4'
for i in range(len(data)):
    if data[i][3] == '4번 이상':
        data[i][3] = '4'
########-------#########


# 참여한 동아리/동문회 활동 종류[4] 구분하고, 번호 매겨놓기
# 주어진 데이터는 ;로 구분되는 데이터
# e.g. '식사 (동아리원과 함께한 모든 식사자리);술자리 (소규모 포함);MT'
# (1) : 식사 (동아리원과 함께한 모든 식사자리)
# (2) : 술자리 (소규모 포함)
# (3) : MT
# (4) : 단체 공연 연습 (합주, 합숙)
# (5) : 전시회 준비 (축제 준비를 위한 전시회 준비)
# (6) : 야외 운동 (축구, 농구, 야구, 요트 등)
# (7) : 기타 야외 활동 (출사, 축제부스, 야외봉사활동 등)
# (8) : 실내 운동 (볼링, 검도 등)
# (9) : 기타 실내활동 (요리, 보드게임, 그림그리기, 실내봉사활동 등)
# (10) : 비대면 활동 (종교활동 등)
# (11) : 동아리 활동을 하지 않음
for i in range(1, len(data)):
    # 주어진 데이터가 ;로 구분되니, 이를 기준으로 나누어 list 생성하기
    cache = list(data[i][4].split(';'))
    # 한글 텍스트를 숫자/NA로 변경하기
    for index_data_cache in range(len(cache)):
        cache_2 = cache[index_data_cache]
        if cache_2 == '식사 (동아리원과 함께한 모든 식사자리)':
            cache[index_data_cache] = '1'
        elif cache_2 == '술자리 (소규모 포함)':
            cache[index_data_cache] = '2'
        elif cache_2 == 'MT':
            cache[index_data_cache] = '3'
        elif cache_2 == '단체 공연 연습 (합주, 합숙)':
            cache[index_data_cache] = '4'
        elif cache_2 == '전시회 준비 (축제 준비를 위한 전시회 준비)':
            cache[index_data_cache] = '5'
        elif cache_2 == '야외 운동 (축구, 농구, 야구, 요트 등)':
            cache[index_data_cache] = '6'
        elif cache_2 == '기타 야외 활동 (출사, 축제부스, 야외봉사활동 등)':
            cache[index_data_cache] = '7'
        elif cache_2 == '실내 운동 (볼링, 검도 등)':
            cache[index_data_cache] = '8'
        elif cache_2 == '기타 실내활동 (요리, 보드게임, 그림그리기, 실내봉사활동 등)':
            cache[index_data_cache] = '9'
        elif cache_2 == '비대면 활동 (종교활동 등)':
            cache[index_data_cache] = '10'
        elif cache_2 == '동아리 활동을 하지 않음':
            cache[index_data_cache] = '11'
    # cache에서 바꾼 각 데이터 다시 합쳐서 data에 입력해주기
    data[i] = data[i][0:4]
    for _ in range(12):
        data[i].append(0)
    for cached_cache in cache:
        data[i][int(cached_cache) + 3] = 1
    data[i][-1] = len(cache) - data[i][14]
    # 각 data[i][4:15]에 참여한 적이 있으면 1, 없으면 0으로 저장됨
        

# 수정된 데이터 다시 csv로 바꿔서 파일 생성하기
file_dir = 'C:\\Users\\bb\\Desktop\\COVID19AndCollegeClubActivity\\revised_data.csv'
f = open(file_dir, 'w', newline='', encoding='utf-8')
w = csv.writer(f)
# 일단, 파일의 첫 행을 '행의 이름'으로 지정해주기
data[0] = ['school_year'                     #[0]
           , 'year_of_birth'                 #[1]
           , 'number_of_participated_club'   #[2]
           , 'number_of_COVID19_infected'    #[3]

           , 'activity_code_1_participated'  #[4]
           , 'activity_code_2_participated'  #[5]
           , 'activity_code_3_participated'  #[6]
           , 'activity_code_4_participated'  #[7]
           , 'activity_code_5_participated'  #[8]
           , 'activity_code_6_participated'  #[9]
           , 'activity_code_7_participated'  #[10]
           , 'activity_code_8_participated'  #[11]
           , 'activity_code_9_participated'  #[12]
           , 'activity_code_10_participated' #[13]
           , 'activity_code_11_participated' #[14]
           , 'total_activity_number'         #[15]
           ]
# data 변수를 csv에 작성하기
w.writerows(data)
f.close()
