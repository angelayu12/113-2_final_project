import pandas as pd
import neurokit2 as nk
from pathlib import Path

# === 參數設定 ===
root_dir       = Path(r"C:\Users\angel\OneDrive\桌面\exam_stress_analysis_for_final_report")   # 放 S1…S10 的資料夾
session_folder = "Final"                                          # 欲分析的次資料夾
csv_name       = "IBI.csv"                                            # 每位受試的 IBI 檔名
sampling_rate  = 64                                                   # RR→peak 時使用的採樣率 (Hz)
output_excel   = root_dir / "final_HRV.xlsx"                       # 最終輸出檔

# 想保留的 HRV 指標（時域 + 頻域比例 / 比值）
wanted_cols = {
    "HRV_RMSSD": "RMSSD",
    "HRV_pNN50": "pNN50",
    "HRV_LFHF":  "LF_HF_Ratio",
}

def compute_selected_hrv(ibi_path: Path) -> pd.Series:
    """給定 IBI.csv 路徑，回傳選定 HRV 指標（Series）。"""
    # 1) 只讀第二欄（IBI），跳過檔案最上方 header
    #    header=None：不把任何列當作欄名
    #    usecols=[1]：只取第 2 欄
    #    names=['ibi']：重新命名這一欄為 'ibi'
    df = pd.read_csv(ibi_path, header=None, usecols=[1], names=['ibi'])
    # 2) 跳過第一列（原本的 'IBI' 標題）
    ibi_series = df['ibi'].iloc[1:].astype(float).reset_index(drop=True)

    # 3) IBI (s) → RR (ms)
    rr_ms = ibi_series.values * 1000

    # 4) RR → R-peak indices
    peaks = nk.intervals_to_peaks(rr_ms, sampling_rate=sampling_rate)

    # 5) HRV 計算（只要時域和頻域）
    time_df = nk.hrv_time(peaks,  sampling_rate=sampling_rate, show=False)
    freq_df = nk.hrv_frequency(peaks, sampling_rate=sampling_rate, show=False)

    # 6) 擷取並重新命名
    merged = pd.concat([time_df, freq_df], axis=1)
    selected = merged[wanted_cols.keys()].rename(columns=wanted_cols)

    # 結果只有一列，轉成 Series 方便往主表合併
    return selected.squeeze()

# === 逐一處理 S1–S10 ===
records = []      # 每位受試者一列
for sid in range(1, 11):           # 1…10
    subj_dir  = root_dir / f"S{sid}" / session_folder
    file_path = subj_dir / csv_name
    try:
        hrv_series = compute_selected_hrv(file_path)
        hrv_series["Subject"] = f"S{sid}"          # 加受試者編號欄
        records.append(hrv_series)
        print(f"✓ 完成 S{sid}")
    except FileNotFoundError:
        print(f"✗ 找不到檔案：{file_path}")
    except Exception as e:
        print(f"✗ {file_path} 計算失敗：{e}")

# === 匯出 Excel ===
if records:   # 至少成功一筆才輸出
    result_df = pd.DataFrame(records).set_index("Subject")
    # 按你想要的欄位順序排好
    result_df = result_df[["RMSSD", "pNN50", "LF_HF_Ratio"]]
    result_df.to_excel(output_excel)
    print(f"已輸出：{output_excel}")
else:
    print("沒有任何檔案成功計算，請檢查路徑與資料格式。")
