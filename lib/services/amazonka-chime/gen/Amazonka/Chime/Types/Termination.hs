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
-- Module      : Amazonka.Chime.Types.Termination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Termination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Termination settings enable your SIP hosts to make outbound calls using
-- your Amazon Chime Voice Connector.
--
-- /See:/ 'newTermination' smart constructor.
data Termination = Termination'
  { -- | The default caller ID phone number.
    defaultPhoneNumber :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | When termination settings are disabled, outbound calls can not be made.
    disabled :: Prelude.Maybe Prelude.Bool,
    -- | The countries to which calls are allowed, in ISO 3166-1 alpha-2 format.
    -- Required.
    callingRegions :: Prelude.Maybe [Prelude.Text],
    -- | The limit on calls per second. Max value based on account service quota.
    -- Default value of 1.
    cpsLimit :: Prelude.Maybe Prelude.Natural,
    -- | The IP addresses allowed to make calls, in CIDR format. Required.
    cidrAllowedList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Termination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultPhoneNumber', 'termination_defaultPhoneNumber' - The default caller ID phone number.
--
-- 'disabled', 'termination_disabled' - When termination settings are disabled, outbound calls can not be made.
--
-- 'callingRegions', 'termination_callingRegions' - The countries to which calls are allowed, in ISO 3166-1 alpha-2 format.
-- Required.
--
-- 'cpsLimit', 'termination_cpsLimit' - The limit on calls per second. Max value based on account service quota.
-- Default value of 1.
--
-- 'cidrAllowedList', 'termination_cidrAllowedList' - The IP addresses allowed to make calls, in CIDR format. Required.
newTermination ::
  Termination
newTermination =
  Termination'
    { defaultPhoneNumber = Prelude.Nothing,
      disabled = Prelude.Nothing,
      callingRegions = Prelude.Nothing,
      cpsLimit = Prelude.Nothing,
      cidrAllowedList = Prelude.Nothing
    }

-- | The default caller ID phone number.
termination_defaultPhoneNumber :: Lens.Lens' Termination (Prelude.Maybe Prelude.Text)
termination_defaultPhoneNumber = Lens.lens (\Termination' {defaultPhoneNumber} -> defaultPhoneNumber) (\s@Termination' {} a -> s {defaultPhoneNumber = a} :: Termination) Prelude.. Lens.mapping Core._Sensitive

-- | When termination settings are disabled, outbound calls can not be made.
termination_disabled :: Lens.Lens' Termination (Prelude.Maybe Prelude.Bool)
termination_disabled = Lens.lens (\Termination' {disabled} -> disabled) (\s@Termination' {} a -> s {disabled = a} :: Termination)

-- | The countries to which calls are allowed, in ISO 3166-1 alpha-2 format.
-- Required.
termination_callingRegions :: Lens.Lens' Termination (Prelude.Maybe [Prelude.Text])
termination_callingRegions = Lens.lens (\Termination' {callingRegions} -> callingRegions) (\s@Termination' {} a -> s {callingRegions = a} :: Termination) Prelude.. Lens.mapping Lens.coerced

-- | The limit on calls per second. Max value based on account service quota.
-- Default value of 1.
termination_cpsLimit :: Lens.Lens' Termination (Prelude.Maybe Prelude.Natural)
termination_cpsLimit = Lens.lens (\Termination' {cpsLimit} -> cpsLimit) (\s@Termination' {} a -> s {cpsLimit = a} :: Termination)

-- | The IP addresses allowed to make calls, in CIDR format. Required.
termination_cidrAllowedList :: Lens.Lens' Termination (Prelude.Maybe [Prelude.Text])
termination_cidrAllowedList = Lens.lens (\Termination' {cidrAllowedList} -> cidrAllowedList) (\s@Termination' {} a -> s {cidrAllowedList = a} :: Termination) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Termination where
  parseJSON =
    Core.withObject
      "Termination"
      ( \x ->
          Termination'
            Prelude.<$> (x Core..:? "DefaultPhoneNumber")
            Prelude.<*> (x Core..:? "Disabled")
            Prelude.<*> (x Core..:? "CallingRegions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CpsLimit")
            Prelude.<*> ( x Core..:? "CidrAllowedList"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Termination where
  hashWithSalt salt' Termination' {..} =
    salt' `Prelude.hashWithSalt` cidrAllowedList
      `Prelude.hashWithSalt` cpsLimit
      `Prelude.hashWithSalt` callingRegions
      `Prelude.hashWithSalt` disabled
      `Prelude.hashWithSalt` defaultPhoneNumber

instance Prelude.NFData Termination where
  rnf Termination' {..} =
    Prelude.rnf defaultPhoneNumber
      `Prelude.seq` Prelude.rnf cidrAllowedList
      `Prelude.seq` Prelude.rnf cpsLimit
      `Prelude.seq` Prelude.rnf callingRegions
      `Prelude.seq` Prelude.rnf disabled

instance Core.ToJSON Termination where
  toJSON Termination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DefaultPhoneNumber" Core..=)
              Prelude.<$> defaultPhoneNumber,
            ("Disabled" Core..=) Prelude.<$> disabled,
            ("CallingRegions" Core..=)
              Prelude.<$> callingRegions,
            ("CpsLimit" Core..=) Prelude.<$> cpsLimit,
            ("CidrAllowedList" Core..=)
              Prelude.<$> cidrAllowedList
          ]
      )
