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
-- Module      : Amazonka.SESV2.Types.BlacklistEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.BlacklistEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about a blacklisting event that
-- impacts one of the dedicated IP addresses that is associated with your
-- account.
--
-- /See:/ 'newBlacklistEntry' smart constructor.
data BlacklistEntry = BlacklistEntry'
  { -- | The time when the blacklisting event occurred, shown in Unix time
    -- format.
    listingTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the blacklist that the IP address appears on.
    rblName :: Prelude.Maybe Prelude.Text,
    -- | Additional information about the blacklisting event, as provided by the
    -- blacklist maintainer.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BlacklistEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listingTime', 'blacklistEntry_listingTime' - The time when the blacklisting event occurred, shown in Unix time
-- format.
--
-- 'rblName', 'blacklistEntry_rblName' - The name of the blacklist that the IP address appears on.
--
-- 'description', 'blacklistEntry_description' - Additional information about the blacklisting event, as provided by the
-- blacklist maintainer.
newBlacklistEntry ::
  BlacklistEntry
newBlacklistEntry =
  BlacklistEntry'
    { listingTime = Prelude.Nothing,
      rblName = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The time when the blacklisting event occurred, shown in Unix time
-- format.
blacklistEntry_listingTime :: Lens.Lens' BlacklistEntry (Prelude.Maybe Prelude.UTCTime)
blacklistEntry_listingTime = Lens.lens (\BlacklistEntry' {listingTime} -> listingTime) (\s@BlacklistEntry' {} a -> s {listingTime = a} :: BlacklistEntry) Prelude.. Lens.mapping Core._Time

-- | The name of the blacklist that the IP address appears on.
blacklistEntry_rblName :: Lens.Lens' BlacklistEntry (Prelude.Maybe Prelude.Text)
blacklistEntry_rblName = Lens.lens (\BlacklistEntry' {rblName} -> rblName) (\s@BlacklistEntry' {} a -> s {rblName = a} :: BlacklistEntry)

-- | Additional information about the blacklisting event, as provided by the
-- blacklist maintainer.
blacklistEntry_description :: Lens.Lens' BlacklistEntry (Prelude.Maybe Prelude.Text)
blacklistEntry_description = Lens.lens (\BlacklistEntry' {description} -> description) (\s@BlacklistEntry' {} a -> s {description = a} :: BlacklistEntry)

instance Core.FromJSON BlacklistEntry where
  parseJSON =
    Core.withObject
      "BlacklistEntry"
      ( \x ->
          BlacklistEntry'
            Prelude.<$> (x Core..:? "ListingTime")
            Prelude.<*> (x Core..:? "RblName")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable BlacklistEntry where
  hashWithSalt salt' BlacklistEntry' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` rblName
      `Prelude.hashWithSalt` listingTime

instance Prelude.NFData BlacklistEntry where
  rnf BlacklistEntry' {..} =
    Prelude.rnf listingTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf rblName
