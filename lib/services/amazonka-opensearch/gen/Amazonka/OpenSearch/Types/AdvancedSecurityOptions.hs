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
-- Module      : Amazonka.OpenSearch.Types.AdvancedSecurityOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AdvancedSecurityOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.SAMLOptionsOutput
import qualified Amazonka.Prelude as Prelude

-- | The advanced security configuration: whether advanced security is
-- enabled, whether the internal database option is enabled.
--
-- /See:/ 'newAdvancedSecurityOptions' smart constructor.
data AdvancedSecurityOptions = AdvancedSecurityOptions'
  { -- | True if advanced security is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | True if the internal user database is enabled.
    internalUserDatabaseEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Describes the SAML application configured for a domain.
    sAMLOptions :: Prelude.Maybe SAMLOptionsOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdvancedSecurityOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'advancedSecurityOptions_enabled' - True if advanced security is enabled.
--
-- 'internalUserDatabaseEnabled', 'advancedSecurityOptions_internalUserDatabaseEnabled' - True if the internal user database is enabled.
--
-- 'sAMLOptions', 'advancedSecurityOptions_sAMLOptions' - Describes the SAML application configured for a domain.
newAdvancedSecurityOptions ::
  AdvancedSecurityOptions
newAdvancedSecurityOptions =
  AdvancedSecurityOptions'
    { enabled = Prelude.Nothing,
      internalUserDatabaseEnabled = Prelude.Nothing,
      sAMLOptions = Prelude.Nothing
    }

-- | True if advanced security is enabled.
advancedSecurityOptions_enabled :: Lens.Lens' AdvancedSecurityOptions (Prelude.Maybe Prelude.Bool)
advancedSecurityOptions_enabled = Lens.lens (\AdvancedSecurityOptions' {enabled} -> enabled) (\s@AdvancedSecurityOptions' {} a -> s {enabled = a} :: AdvancedSecurityOptions)

-- | True if the internal user database is enabled.
advancedSecurityOptions_internalUserDatabaseEnabled :: Lens.Lens' AdvancedSecurityOptions (Prelude.Maybe Prelude.Bool)
advancedSecurityOptions_internalUserDatabaseEnabled = Lens.lens (\AdvancedSecurityOptions' {internalUserDatabaseEnabled} -> internalUserDatabaseEnabled) (\s@AdvancedSecurityOptions' {} a -> s {internalUserDatabaseEnabled = a} :: AdvancedSecurityOptions)

-- | Describes the SAML application configured for a domain.
advancedSecurityOptions_sAMLOptions :: Lens.Lens' AdvancedSecurityOptions (Prelude.Maybe SAMLOptionsOutput)
advancedSecurityOptions_sAMLOptions = Lens.lens (\AdvancedSecurityOptions' {sAMLOptions} -> sAMLOptions) (\s@AdvancedSecurityOptions' {} a -> s {sAMLOptions = a} :: AdvancedSecurityOptions)

instance Core.FromJSON AdvancedSecurityOptions where
  parseJSON =
    Core.withObject
      "AdvancedSecurityOptions"
      ( \x ->
          AdvancedSecurityOptions'
            Prelude.<$> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "InternalUserDatabaseEnabled")
            Prelude.<*> (x Core..:? "SAMLOptions")
      )

instance Prelude.Hashable AdvancedSecurityOptions where
  hashWithSalt salt' AdvancedSecurityOptions' {..} =
    salt' `Prelude.hashWithSalt` sAMLOptions
      `Prelude.hashWithSalt` internalUserDatabaseEnabled
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData AdvancedSecurityOptions where
  rnf AdvancedSecurityOptions' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf sAMLOptions
      `Prelude.seq` Prelude.rnf internalUserDatabaseEnabled
