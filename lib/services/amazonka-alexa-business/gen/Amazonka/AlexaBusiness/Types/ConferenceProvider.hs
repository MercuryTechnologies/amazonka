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
-- Module      : Amazonka.AlexaBusiness.Types.ConferenceProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.ConferenceProvider where

import Amazonka.AlexaBusiness.Types.ConferenceProviderType
import Amazonka.AlexaBusiness.Types.IPDialIn
import Amazonka.AlexaBusiness.Types.MeetingSetting
import Amazonka.AlexaBusiness.Types.PSTNDialIn
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An entity that provides a conferencing solution. Alexa for Business acts
-- as the voice interface and mediator that connects users to their
-- preferred conference provider. Examples of conference providers include
-- Amazon Chime, Zoom, Cisco, and Polycom.
--
-- /See:/ 'newConferenceProvider' smart constructor.
data ConferenceProvider = ConferenceProvider'
  { -- | The meeting settings for the conference provider.
    meetingSetting :: Prelude.Maybe MeetingSetting,
    -- | The ARN of the newly created conference provider.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The information for PSTN conferencing.
    pSTNDialIn :: Prelude.Maybe PSTNDialIn,
    -- | The name of the conference provider.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of conference providers.
    type' :: Prelude.Maybe ConferenceProviderType,
    -- | The IP endpoint and protocol for calling.
    iPDialIn :: Prelude.Maybe IPDialIn
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConferenceProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingSetting', 'conferenceProvider_meetingSetting' - The meeting settings for the conference provider.
--
-- 'arn', 'conferenceProvider_arn' - The ARN of the newly created conference provider.
--
-- 'pSTNDialIn', 'conferenceProvider_pSTNDialIn' - The information for PSTN conferencing.
--
-- 'name', 'conferenceProvider_name' - The name of the conference provider.
--
-- 'type'', 'conferenceProvider_type' - The type of conference providers.
--
-- 'iPDialIn', 'conferenceProvider_iPDialIn' - The IP endpoint and protocol for calling.
newConferenceProvider ::
  ConferenceProvider
newConferenceProvider =
  ConferenceProvider'
    { meetingSetting =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      pSTNDialIn = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      iPDialIn = Prelude.Nothing
    }

-- | The meeting settings for the conference provider.
conferenceProvider_meetingSetting :: Lens.Lens' ConferenceProvider (Prelude.Maybe MeetingSetting)
conferenceProvider_meetingSetting = Lens.lens (\ConferenceProvider' {meetingSetting} -> meetingSetting) (\s@ConferenceProvider' {} a -> s {meetingSetting = a} :: ConferenceProvider)

-- | The ARN of the newly created conference provider.
conferenceProvider_arn :: Lens.Lens' ConferenceProvider (Prelude.Maybe Prelude.Text)
conferenceProvider_arn = Lens.lens (\ConferenceProvider' {arn} -> arn) (\s@ConferenceProvider' {} a -> s {arn = a} :: ConferenceProvider)

-- | The information for PSTN conferencing.
conferenceProvider_pSTNDialIn :: Lens.Lens' ConferenceProvider (Prelude.Maybe PSTNDialIn)
conferenceProvider_pSTNDialIn = Lens.lens (\ConferenceProvider' {pSTNDialIn} -> pSTNDialIn) (\s@ConferenceProvider' {} a -> s {pSTNDialIn = a} :: ConferenceProvider)

-- | The name of the conference provider.
conferenceProvider_name :: Lens.Lens' ConferenceProvider (Prelude.Maybe Prelude.Text)
conferenceProvider_name = Lens.lens (\ConferenceProvider' {name} -> name) (\s@ConferenceProvider' {} a -> s {name = a} :: ConferenceProvider)

-- | The type of conference providers.
conferenceProvider_type :: Lens.Lens' ConferenceProvider (Prelude.Maybe ConferenceProviderType)
conferenceProvider_type = Lens.lens (\ConferenceProvider' {type'} -> type') (\s@ConferenceProvider' {} a -> s {type' = a} :: ConferenceProvider)

-- | The IP endpoint and protocol for calling.
conferenceProvider_iPDialIn :: Lens.Lens' ConferenceProvider (Prelude.Maybe IPDialIn)
conferenceProvider_iPDialIn = Lens.lens (\ConferenceProvider' {iPDialIn} -> iPDialIn) (\s@ConferenceProvider' {} a -> s {iPDialIn = a} :: ConferenceProvider)

instance Core.FromJSON ConferenceProvider where
  parseJSON =
    Core.withObject
      "ConferenceProvider"
      ( \x ->
          ConferenceProvider'
            Prelude.<$> (x Core..:? "MeetingSetting")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "PSTNDialIn")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "IPDialIn")
      )

instance Prelude.Hashable ConferenceProvider where
  hashWithSalt salt' ConferenceProvider' {..} =
    salt' `Prelude.hashWithSalt` iPDialIn
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` pSTNDialIn
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` meetingSetting

instance Prelude.NFData ConferenceProvider where
  rnf ConferenceProvider' {..} =
    Prelude.rnf meetingSetting
      `Prelude.seq` Prelude.rnf iPDialIn
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf pSTNDialIn
      `Prelude.seq` Prelude.rnf arn
