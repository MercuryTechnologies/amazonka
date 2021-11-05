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
-- Module      : Amazonka.SageMaker.Types.DomainDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DomainDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.DomainStatus

-- | The domain\'s details.
--
-- /See:/ 'newDomainDetails' smart constructor.
data DomainDetails = DomainDetails'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status.
    status :: Prelude.Maybe DomainStatus,
    -- | The domain\'s Amazon Resource Name (ARN).
    domainArn :: Prelude.Maybe Prelude.Text,
    -- | The domain\'s URL.
    url :: Prelude.Maybe Prelude.Text,
    -- | The last modified time.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The domain name.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The domain ID.
    domainId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'domainDetails_creationTime' - The creation time.
--
-- 'status', 'domainDetails_status' - The status.
--
-- 'domainArn', 'domainDetails_domainArn' - The domain\'s Amazon Resource Name (ARN).
--
-- 'url', 'domainDetails_url' - The domain\'s URL.
--
-- 'lastModifiedTime', 'domainDetails_lastModifiedTime' - The last modified time.
--
-- 'domainName', 'domainDetails_domainName' - The domain name.
--
-- 'domainId', 'domainDetails_domainId' - The domain ID.
newDomainDetails ::
  DomainDetails
newDomainDetails =
  DomainDetails'
    { creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      domainArn = Prelude.Nothing,
      url = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      domainName = Prelude.Nothing,
      domainId = Prelude.Nothing
    }

-- | The creation time.
domainDetails_creationTime :: Lens.Lens' DomainDetails (Prelude.Maybe Prelude.UTCTime)
domainDetails_creationTime = Lens.lens (\DomainDetails' {creationTime} -> creationTime) (\s@DomainDetails' {} a -> s {creationTime = a} :: DomainDetails) Prelude.. Lens.mapping Core._Time

-- | The status.
domainDetails_status :: Lens.Lens' DomainDetails (Prelude.Maybe DomainStatus)
domainDetails_status = Lens.lens (\DomainDetails' {status} -> status) (\s@DomainDetails' {} a -> s {status = a} :: DomainDetails)

-- | The domain\'s Amazon Resource Name (ARN).
domainDetails_domainArn :: Lens.Lens' DomainDetails (Prelude.Maybe Prelude.Text)
domainDetails_domainArn = Lens.lens (\DomainDetails' {domainArn} -> domainArn) (\s@DomainDetails' {} a -> s {domainArn = a} :: DomainDetails)

-- | The domain\'s URL.
domainDetails_url :: Lens.Lens' DomainDetails (Prelude.Maybe Prelude.Text)
domainDetails_url = Lens.lens (\DomainDetails' {url} -> url) (\s@DomainDetails' {} a -> s {url = a} :: DomainDetails)

-- | The last modified time.
domainDetails_lastModifiedTime :: Lens.Lens' DomainDetails (Prelude.Maybe Prelude.UTCTime)
domainDetails_lastModifiedTime = Lens.lens (\DomainDetails' {lastModifiedTime} -> lastModifiedTime) (\s@DomainDetails' {} a -> s {lastModifiedTime = a} :: DomainDetails) Prelude.. Lens.mapping Core._Time

-- | The domain name.
domainDetails_domainName :: Lens.Lens' DomainDetails (Prelude.Maybe Prelude.Text)
domainDetails_domainName = Lens.lens (\DomainDetails' {domainName} -> domainName) (\s@DomainDetails' {} a -> s {domainName = a} :: DomainDetails)

-- | The domain ID.
domainDetails_domainId :: Lens.Lens' DomainDetails (Prelude.Maybe Prelude.Text)
domainDetails_domainId = Lens.lens (\DomainDetails' {domainId} -> domainId) (\s@DomainDetails' {} a -> s {domainId = a} :: DomainDetails)

instance Core.FromJSON DomainDetails where
  parseJSON =
    Core.withObject
      "DomainDetails"
      ( \x ->
          DomainDetails'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "DomainArn")
            Prelude.<*> (x Core..:? "Url")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "DomainName")
            Prelude.<*> (x Core..:? "DomainId")
      )

instance Prelude.Hashable DomainDetails

instance Prelude.NFData DomainDetails
