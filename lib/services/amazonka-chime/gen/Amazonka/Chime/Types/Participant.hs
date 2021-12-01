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
-- Module      : Amazonka.Chime.Types.Participant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Participant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The phone number and proxy phone number for a participant in an Amazon
-- Chime Voice Connector proxy session.
--
-- /See:/ 'newParticipant' smart constructor.
data Participant = Participant'
  { -- | The participant\'s phone number.
    phoneNumber :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The participant\'s proxy phone number.
    proxyPhoneNumber :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Participant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'participant_phoneNumber' - The participant\'s phone number.
--
-- 'proxyPhoneNumber', 'participant_proxyPhoneNumber' - The participant\'s proxy phone number.
newParticipant ::
  Participant
newParticipant =
  Participant'
    { phoneNumber = Prelude.Nothing,
      proxyPhoneNumber = Prelude.Nothing
    }

-- | The participant\'s phone number.
participant_phoneNumber :: Lens.Lens' Participant (Prelude.Maybe Prelude.Text)
participant_phoneNumber = Lens.lens (\Participant' {phoneNumber} -> phoneNumber) (\s@Participant' {} a -> s {phoneNumber = a} :: Participant) Prelude.. Lens.mapping Core._Sensitive

-- | The participant\'s proxy phone number.
participant_proxyPhoneNumber :: Lens.Lens' Participant (Prelude.Maybe Prelude.Text)
participant_proxyPhoneNumber = Lens.lens (\Participant' {proxyPhoneNumber} -> proxyPhoneNumber) (\s@Participant' {} a -> s {proxyPhoneNumber = a} :: Participant) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON Participant where
  parseJSON =
    Core.withObject
      "Participant"
      ( \x ->
          Participant'
            Prelude.<$> (x Core..:? "PhoneNumber")
            Prelude.<*> (x Core..:? "ProxyPhoneNumber")
      )

instance Prelude.Hashable Participant where
  hashWithSalt salt' Participant' {..} =
    salt' `Prelude.hashWithSalt` proxyPhoneNumber
      `Prelude.hashWithSalt` phoneNumber

instance Prelude.NFData Participant where
  rnf Participant' {..} =
    Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf proxyPhoneNumber
