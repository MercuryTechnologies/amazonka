{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AlexaBusiness.UpdateProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing room profile by room profile ARN.
module Amazonka.AlexaBusiness.UpdateProfile
  ( -- * Creating a Request
    UpdateProfile (..),
    newUpdateProfile,

    -- * Request Lenses
    updateProfile_setupModeDisabled,
    updateProfile_pSTNEnabled,
    updateProfile_distanceUnit,
    updateProfile_locale,
    updateProfile_address,
    updateProfile_profileArn,
    updateProfile_wakeWord,
    updateProfile_meetingRoomConfiguration,
    updateProfile_profileName,
    updateProfile_temperatureUnit,
    updateProfile_dataRetentionOptIn,
    updateProfile_timezone,
    updateProfile_maxVolumeLimit,
    updateProfile_isDefault,

    -- * Destructuring the Response
    UpdateProfileResponse (..),
    newUpdateProfileResponse,

    -- * Response Lenses
    updateProfileResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProfile' smart constructor.
data UpdateProfile = UpdateProfile'
  { -- | Whether the setup mode of the profile is enabled.
    setupModeDisabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the PSTN setting of the room profile is enabled.
    pSTNEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The updated distance unit for the room profile.
    distanceUnit :: Prelude.Maybe DistanceUnit,
    -- | The updated locale for the room profile. (This is currently only
    -- available to a limited preview audience.)
    locale :: Prelude.Maybe Prelude.Text,
    -- | The updated address for the room profile.
    address :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the room profile to update. Required.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The updated wake word for the room profile.
    wakeWord :: Prelude.Maybe WakeWord,
    -- | The updated meeting room settings of a room profile.
    meetingRoomConfiguration :: Prelude.Maybe UpdateMeetingRoomConfiguration,
    -- | The updated name for the room profile.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | The updated temperature unit for the room profile.
    temperatureUnit :: Prelude.Maybe TemperatureUnit,
    -- | Whether data retention of the profile is enabled.
    dataRetentionOptIn :: Prelude.Maybe Prelude.Bool,
    -- | The updated timezone for the room profile.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The updated maximum volume limit for the room profile.
    maxVolumeLimit :: Prelude.Maybe Prelude.Int,
    -- | Sets the profile as default if selected. If this is missing, no update
    -- is done to the default status.
    isDefault :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setupModeDisabled', 'updateProfile_setupModeDisabled' - Whether the setup mode of the profile is enabled.
--
-- 'pSTNEnabled', 'updateProfile_pSTNEnabled' - Whether the PSTN setting of the room profile is enabled.
--
-- 'distanceUnit', 'updateProfile_distanceUnit' - The updated distance unit for the room profile.
--
-- 'locale', 'updateProfile_locale' - The updated locale for the room profile. (This is currently only
-- available to a limited preview audience.)
--
-- 'address', 'updateProfile_address' - The updated address for the room profile.
--
-- 'profileArn', 'updateProfile_profileArn' - The ARN of the room profile to update. Required.
--
-- 'wakeWord', 'updateProfile_wakeWord' - The updated wake word for the room profile.
--
-- 'meetingRoomConfiguration', 'updateProfile_meetingRoomConfiguration' - The updated meeting room settings of a room profile.
--
-- 'profileName', 'updateProfile_profileName' - The updated name for the room profile.
--
-- 'temperatureUnit', 'updateProfile_temperatureUnit' - The updated temperature unit for the room profile.
--
-- 'dataRetentionOptIn', 'updateProfile_dataRetentionOptIn' - Whether data retention of the profile is enabled.
--
-- 'timezone', 'updateProfile_timezone' - The updated timezone for the room profile.
--
-- 'maxVolumeLimit', 'updateProfile_maxVolumeLimit' - The updated maximum volume limit for the room profile.
--
-- 'isDefault', 'updateProfile_isDefault' - Sets the profile as default if selected. If this is missing, no update
-- is done to the default status.
newUpdateProfile ::
  UpdateProfile
newUpdateProfile =
  UpdateProfile'
    { setupModeDisabled = Prelude.Nothing,
      pSTNEnabled = Prelude.Nothing,
      distanceUnit = Prelude.Nothing,
      locale = Prelude.Nothing,
      address = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      wakeWord = Prelude.Nothing,
      meetingRoomConfiguration = Prelude.Nothing,
      profileName = Prelude.Nothing,
      temperatureUnit = Prelude.Nothing,
      dataRetentionOptIn = Prelude.Nothing,
      timezone = Prelude.Nothing,
      maxVolumeLimit = Prelude.Nothing,
      isDefault = Prelude.Nothing
    }

-- | Whether the setup mode of the profile is enabled.
updateProfile_setupModeDisabled :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Bool)
updateProfile_setupModeDisabled = Lens.lens (\UpdateProfile' {setupModeDisabled} -> setupModeDisabled) (\s@UpdateProfile' {} a -> s {setupModeDisabled = a} :: UpdateProfile)

-- | Whether the PSTN setting of the room profile is enabled.
updateProfile_pSTNEnabled :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Bool)
updateProfile_pSTNEnabled = Lens.lens (\UpdateProfile' {pSTNEnabled} -> pSTNEnabled) (\s@UpdateProfile' {} a -> s {pSTNEnabled = a} :: UpdateProfile)

-- | The updated distance unit for the room profile.
updateProfile_distanceUnit :: Lens.Lens' UpdateProfile (Prelude.Maybe DistanceUnit)
updateProfile_distanceUnit = Lens.lens (\UpdateProfile' {distanceUnit} -> distanceUnit) (\s@UpdateProfile' {} a -> s {distanceUnit = a} :: UpdateProfile)

-- | The updated locale for the room profile. (This is currently only
-- available to a limited preview audience.)
updateProfile_locale :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_locale = Lens.lens (\UpdateProfile' {locale} -> locale) (\s@UpdateProfile' {} a -> s {locale = a} :: UpdateProfile)

-- | The updated address for the room profile.
updateProfile_address :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_address = Lens.lens (\UpdateProfile' {address} -> address) (\s@UpdateProfile' {} a -> s {address = a} :: UpdateProfile)

-- | The ARN of the room profile to update. Required.
updateProfile_profileArn :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_profileArn = Lens.lens (\UpdateProfile' {profileArn} -> profileArn) (\s@UpdateProfile' {} a -> s {profileArn = a} :: UpdateProfile)

-- | The updated wake word for the room profile.
updateProfile_wakeWord :: Lens.Lens' UpdateProfile (Prelude.Maybe WakeWord)
updateProfile_wakeWord = Lens.lens (\UpdateProfile' {wakeWord} -> wakeWord) (\s@UpdateProfile' {} a -> s {wakeWord = a} :: UpdateProfile)

-- | The updated meeting room settings of a room profile.
updateProfile_meetingRoomConfiguration :: Lens.Lens' UpdateProfile (Prelude.Maybe UpdateMeetingRoomConfiguration)
updateProfile_meetingRoomConfiguration = Lens.lens (\UpdateProfile' {meetingRoomConfiguration} -> meetingRoomConfiguration) (\s@UpdateProfile' {} a -> s {meetingRoomConfiguration = a} :: UpdateProfile)

-- | The updated name for the room profile.
updateProfile_profileName :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_profileName = Lens.lens (\UpdateProfile' {profileName} -> profileName) (\s@UpdateProfile' {} a -> s {profileName = a} :: UpdateProfile)

-- | The updated temperature unit for the room profile.
updateProfile_temperatureUnit :: Lens.Lens' UpdateProfile (Prelude.Maybe TemperatureUnit)
updateProfile_temperatureUnit = Lens.lens (\UpdateProfile' {temperatureUnit} -> temperatureUnit) (\s@UpdateProfile' {} a -> s {temperatureUnit = a} :: UpdateProfile)

-- | Whether data retention of the profile is enabled.
updateProfile_dataRetentionOptIn :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Bool)
updateProfile_dataRetentionOptIn = Lens.lens (\UpdateProfile' {dataRetentionOptIn} -> dataRetentionOptIn) (\s@UpdateProfile' {} a -> s {dataRetentionOptIn = a} :: UpdateProfile)

-- | The updated timezone for the room profile.
updateProfile_timezone :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_timezone = Lens.lens (\UpdateProfile' {timezone} -> timezone) (\s@UpdateProfile' {} a -> s {timezone = a} :: UpdateProfile)

-- | The updated maximum volume limit for the room profile.
updateProfile_maxVolumeLimit :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Int)
updateProfile_maxVolumeLimit = Lens.lens (\UpdateProfile' {maxVolumeLimit} -> maxVolumeLimit) (\s@UpdateProfile' {} a -> s {maxVolumeLimit = a} :: UpdateProfile)

-- | Sets the profile as default if selected. If this is missing, no update
-- is done to the default status.
updateProfile_isDefault :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Bool)
updateProfile_isDefault = Lens.lens (\UpdateProfile' {isDefault} -> isDefault) (\s@UpdateProfile' {} a -> s {isDefault = a} :: UpdateProfile)

instance Core.AWSRequest UpdateProfile where
  type
    AWSResponse UpdateProfile =
      UpdateProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProfile where
  hashWithSalt salt' UpdateProfile' {..} =
    salt' `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` maxVolumeLimit
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` dataRetentionOptIn
      `Prelude.hashWithSalt` temperatureUnit
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` meetingRoomConfiguration
      `Prelude.hashWithSalt` wakeWord
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` distanceUnit
      `Prelude.hashWithSalt` pSTNEnabled
      `Prelude.hashWithSalt` setupModeDisabled

instance Prelude.NFData UpdateProfile where
  rnf UpdateProfile' {..} =
    Prelude.rnf setupModeDisabled
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf maxVolumeLimit
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf dataRetentionOptIn
      `Prelude.seq` Prelude.rnf temperatureUnit
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf meetingRoomConfiguration
      `Prelude.seq` Prelude.rnf wakeWord
      `Prelude.seq` Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf address
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf distanceUnit
      `Prelude.seq` Prelude.rnf pSTNEnabled

instance Core.ToHeaders UpdateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.UpdateProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateProfile where
  toJSON UpdateProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SetupModeDisabled" Core..=)
              Prelude.<$> setupModeDisabled,
            ("PSTNEnabled" Core..=) Prelude.<$> pSTNEnabled,
            ("DistanceUnit" Core..=) Prelude.<$> distanceUnit,
            ("Locale" Core..=) Prelude.<$> locale,
            ("Address" Core..=) Prelude.<$> address,
            ("ProfileArn" Core..=) Prelude.<$> profileArn,
            ("WakeWord" Core..=) Prelude.<$> wakeWord,
            ("MeetingRoomConfiguration" Core..=)
              Prelude.<$> meetingRoomConfiguration,
            ("ProfileName" Core..=) Prelude.<$> profileName,
            ("TemperatureUnit" Core..=)
              Prelude.<$> temperatureUnit,
            ("DataRetentionOptIn" Core..=)
              Prelude.<$> dataRetentionOptIn,
            ("Timezone" Core..=) Prelude.<$> timezone,
            ("MaxVolumeLimit" Core..=)
              Prelude.<$> maxVolumeLimit,
            ("IsDefault" Core..=) Prelude.<$> isDefault
          ]
      )

instance Core.ToPath UpdateProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProfileResponse' smart constructor.
data UpdateProfileResponse = UpdateProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateProfileResponse_httpStatus' - The response's http status code.
newUpdateProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProfileResponse
newUpdateProfileResponse pHttpStatus_ =
  UpdateProfileResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateProfileResponse_httpStatus :: Lens.Lens' UpdateProfileResponse Prelude.Int
updateProfileResponse_httpStatus = Lens.lens (\UpdateProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateProfileResponse' {} a -> s {httpStatus = a} :: UpdateProfileResponse)

instance Prelude.NFData UpdateProfileResponse where
  rnf UpdateProfileResponse' {..} =
    Prelude.rnf httpStatus
