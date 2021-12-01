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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.R53ResourceRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.R53ResourceRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Route 53 resource a DNS Target Resource record points to
--
-- /See:/ 'newR53ResourceRecord' smart constructor.
data R53ResourceRecord = R53ResourceRecord'
  { -- | The DNS target name
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The Resource Record set id
    recordSetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'R53ResourceRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'r53ResourceRecord_domainName' - The DNS target name
--
-- 'recordSetId', 'r53ResourceRecord_recordSetId' - The Resource Record set id
newR53ResourceRecord ::
  R53ResourceRecord
newR53ResourceRecord =
  R53ResourceRecord'
    { domainName = Prelude.Nothing,
      recordSetId = Prelude.Nothing
    }

-- | The DNS target name
r53ResourceRecord_domainName :: Lens.Lens' R53ResourceRecord (Prelude.Maybe Prelude.Text)
r53ResourceRecord_domainName = Lens.lens (\R53ResourceRecord' {domainName} -> domainName) (\s@R53ResourceRecord' {} a -> s {domainName = a} :: R53ResourceRecord)

-- | The Resource Record set id
r53ResourceRecord_recordSetId :: Lens.Lens' R53ResourceRecord (Prelude.Maybe Prelude.Text)
r53ResourceRecord_recordSetId = Lens.lens (\R53ResourceRecord' {recordSetId} -> recordSetId) (\s@R53ResourceRecord' {} a -> s {recordSetId = a} :: R53ResourceRecord)

instance Core.FromJSON R53ResourceRecord where
  parseJSON =
    Core.withObject
      "R53ResourceRecord"
      ( \x ->
          R53ResourceRecord'
            Prelude.<$> (x Core..:? "domainName")
            Prelude.<*> (x Core..:? "recordSetId")
      )

instance Prelude.Hashable R53ResourceRecord where
  hashWithSalt salt' R53ResourceRecord' {..} =
    salt' `Prelude.hashWithSalt` recordSetId
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData R53ResourceRecord where
  rnf R53ResourceRecord' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf recordSetId

instance Core.ToJSON R53ResourceRecord where
  toJSON R53ResourceRecord' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("domainName" Core..=) Prelude.<$> domainName,
            ("recordSetId" Core..=) Prelude.<$> recordSetId
          ]
      )
