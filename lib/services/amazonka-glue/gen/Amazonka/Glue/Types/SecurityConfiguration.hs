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
-- Module      : Amazonka.Glue.Types.SecurityConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SecurityConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.EncryptionConfiguration
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a security configuration.
--
-- /See:/ 'newSecurityConfiguration' smart constructor.
data SecurityConfiguration = SecurityConfiguration'
  { -- | The name of the security configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The encryption configuration associated with this security
    -- configuration.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The time at which this security configuration was created.
    createdTimeStamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'securityConfiguration_name' - The name of the security configuration.
--
-- 'encryptionConfiguration', 'securityConfiguration_encryptionConfiguration' - The encryption configuration associated with this security
-- configuration.
--
-- 'createdTimeStamp', 'securityConfiguration_createdTimeStamp' - The time at which this security configuration was created.
newSecurityConfiguration ::
  SecurityConfiguration
newSecurityConfiguration =
  SecurityConfiguration'
    { name = Prelude.Nothing,
      encryptionConfiguration = Prelude.Nothing,
      createdTimeStamp = Prelude.Nothing
    }

-- | The name of the security configuration.
securityConfiguration_name :: Lens.Lens' SecurityConfiguration (Prelude.Maybe Prelude.Text)
securityConfiguration_name = Lens.lens (\SecurityConfiguration' {name} -> name) (\s@SecurityConfiguration' {} a -> s {name = a} :: SecurityConfiguration)

-- | The encryption configuration associated with this security
-- configuration.
securityConfiguration_encryptionConfiguration :: Lens.Lens' SecurityConfiguration (Prelude.Maybe EncryptionConfiguration)
securityConfiguration_encryptionConfiguration = Lens.lens (\SecurityConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@SecurityConfiguration' {} a -> s {encryptionConfiguration = a} :: SecurityConfiguration)

-- | The time at which this security configuration was created.
securityConfiguration_createdTimeStamp :: Lens.Lens' SecurityConfiguration (Prelude.Maybe Prelude.UTCTime)
securityConfiguration_createdTimeStamp = Lens.lens (\SecurityConfiguration' {createdTimeStamp} -> createdTimeStamp) (\s@SecurityConfiguration' {} a -> s {createdTimeStamp = a} :: SecurityConfiguration) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON SecurityConfiguration where
  parseJSON =
    Core.withObject
      "SecurityConfiguration"
      ( \x ->
          SecurityConfiguration'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "EncryptionConfiguration")
            Prelude.<*> (x Core..:? "CreatedTimeStamp")
      )

instance Prelude.Hashable SecurityConfiguration where
  hashWithSalt salt' SecurityConfiguration' {..} =
    salt' `Prelude.hashWithSalt` createdTimeStamp
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` name

instance Prelude.NFData SecurityConfiguration where
  rnf SecurityConfiguration' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTimeStamp
      `Prelude.seq` Prelude.rnf encryptionConfiguration
