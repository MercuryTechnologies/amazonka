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
-- Module      : Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateKeyUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateKeyUsage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a key usage X.509 v3 extension object.
--
-- /See:/ 'newAwsCertificateManagerCertificateKeyUsage' smart constructor.
data AwsCertificateManagerCertificateKeyUsage = AwsCertificateManagerCertificateKeyUsage'
  { -- | The key usage extension name.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCertificateManagerCertificateKeyUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsCertificateManagerCertificateKeyUsage_name' - The key usage extension name.
newAwsCertificateManagerCertificateKeyUsage ::
  AwsCertificateManagerCertificateKeyUsage
newAwsCertificateManagerCertificateKeyUsage =
  AwsCertificateManagerCertificateKeyUsage'
    { name =
        Prelude.Nothing
    }

-- | The key usage extension name.
awsCertificateManagerCertificateKeyUsage_name :: Lens.Lens' AwsCertificateManagerCertificateKeyUsage (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateKeyUsage_name = Lens.lens (\AwsCertificateManagerCertificateKeyUsage' {name} -> name) (\s@AwsCertificateManagerCertificateKeyUsage' {} a -> s {name = a} :: AwsCertificateManagerCertificateKeyUsage)

instance
  Core.FromJSON
    AwsCertificateManagerCertificateKeyUsage
  where
  parseJSON =
    Core.withObject
      "AwsCertificateManagerCertificateKeyUsage"
      ( \x ->
          AwsCertificateManagerCertificateKeyUsage'
            Prelude.<$> (x Core..:? "Name")
      )

instance
  Prelude.Hashable
    AwsCertificateManagerCertificateKeyUsage
  where
  hashWithSalt
    salt'
    AwsCertificateManagerCertificateKeyUsage' {..} =
      salt' `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    AwsCertificateManagerCertificateKeyUsage
  where
  rnf AwsCertificateManagerCertificateKeyUsage' {..} =
    Prelude.rnf name

instance
  Core.ToJSON
    AwsCertificateManagerCertificateKeyUsage
  where
  toJSON AwsCertificateManagerCertificateKeyUsage' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Name" Core..=) Prelude.<$> name]
      )
