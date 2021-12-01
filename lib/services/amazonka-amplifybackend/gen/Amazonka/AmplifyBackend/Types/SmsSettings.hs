{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AmplifyBackend.Types.SmsSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.SmsSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | SMS settings for authentication.
--
-- /See:/ 'newSmsSettings' smart constructor.
data SmsSettings = SmsSettings'
  { -- | The body of the SMS message.
    smsMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SmsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'smsMessage', 'smsSettings_smsMessage' - The body of the SMS message.
newSmsSettings ::
  SmsSettings
newSmsSettings =
  SmsSettings' {smsMessage = Prelude.Nothing}

-- | The body of the SMS message.
smsSettings_smsMessage :: Lens.Lens' SmsSettings (Prelude.Maybe Prelude.Text)
smsSettings_smsMessage = Lens.lens (\SmsSettings' {smsMessage} -> smsMessage) (\s@SmsSettings' {} a -> s {smsMessage = a} :: SmsSettings)

instance Core.FromJSON SmsSettings where
  parseJSON =
    Core.withObject
      "SmsSettings"
      ( \x ->
          SmsSettings' Prelude.<$> (x Core..:? "smsMessage")
      )

instance Prelude.Hashable SmsSettings where
  hashWithSalt salt' SmsSettings' {..} =
    salt' `Prelude.hashWithSalt` smsMessage

instance Prelude.NFData SmsSettings where
  rnf SmsSettings' {..} = Prelude.rnf smsMessage

instance Core.ToJSON SmsSettings where
  toJSON SmsSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [("smsMessage" Core..=) Prelude.<$> smsMessage]
      )
