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
-- Module      : Amazonka.SecurityHub.Types.AwsApiCallAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiCallAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.ActionRemoteIpDetails
import Amazonka.SecurityHub.Types.AwsApiCallActionDomainDetails

-- | Provided if @ActionType@ is @AWS_API_CALL@. It provides details about
-- the API call that was detected.
--
-- /See:/ 'newAwsApiCallAction' smart constructor.
data AwsApiCallAction = AwsApiCallAction'
  { -- | Provided if @CallerType@ is @remoteIp@. Provides information about the
    -- remote IP address that the API call originated from.
    remoteIpDetails :: Prelude.Maybe ActionRemoteIpDetails,
    -- | An ISO8601-formatted timestamp that indicates when the API call was
    -- first observed.
    firstSeen :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the API call originated from a remote IP address
    -- (@remoteip@) or from a DNS domain (@domain@).
    callerType :: Prelude.Maybe Prelude.Text,
    -- | Identifies the resources that were affected by the API call.
    affectedResources :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An ISO8601-formatted timestamp that indicates when the API call was most
    -- recently observed.
    lastSeen :: Prelude.Maybe Prelude.Text,
    -- | Provided if @CallerType@ is @domain@. Provides information about the DNS
    -- domain that the API call originated from.
    domainDetails :: Prelude.Maybe AwsApiCallActionDomainDetails,
    -- | The name of the Amazon Web Services service that the API method belongs
    -- to.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the API method that was issued.
    api :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsApiCallAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteIpDetails', 'awsApiCallAction_remoteIpDetails' - Provided if @CallerType@ is @remoteIp@. Provides information about the
-- remote IP address that the API call originated from.
--
-- 'firstSeen', 'awsApiCallAction_firstSeen' - An ISO8601-formatted timestamp that indicates when the API call was
-- first observed.
--
-- 'callerType', 'awsApiCallAction_callerType' - Indicates whether the API call originated from a remote IP address
-- (@remoteip@) or from a DNS domain (@domain@).
--
-- 'affectedResources', 'awsApiCallAction_affectedResources' - Identifies the resources that were affected by the API call.
--
-- 'lastSeen', 'awsApiCallAction_lastSeen' - An ISO8601-formatted timestamp that indicates when the API call was most
-- recently observed.
--
-- 'domainDetails', 'awsApiCallAction_domainDetails' - Provided if @CallerType@ is @domain@. Provides information about the DNS
-- domain that the API call originated from.
--
-- 'serviceName', 'awsApiCallAction_serviceName' - The name of the Amazon Web Services service that the API method belongs
-- to.
--
-- 'api', 'awsApiCallAction_api' - The name of the API method that was issued.
newAwsApiCallAction ::
  AwsApiCallAction
newAwsApiCallAction =
  AwsApiCallAction'
    { remoteIpDetails =
        Prelude.Nothing,
      firstSeen = Prelude.Nothing,
      callerType = Prelude.Nothing,
      affectedResources = Prelude.Nothing,
      lastSeen = Prelude.Nothing,
      domainDetails = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      api = Prelude.Nothing
    }

-- | Provided if @CallerType@ is @remoteIp@. Provides information about the
-- remote IP address that the API call originated from.
awsApiCallAction_remoteIpDetails :: Lens.Lens' AwsApiCallAction (Prelude.Maybe ActionRemoteIpDetails)
awsApiCallAction_remoteIpDetails = Lens.lens (\AwsApiCallAction' {remoteIpDetails} -> remoteIpDetails) (\s@AwsApiCallAction' {} a -> s {remoteIpDetails = a} :: AwsApiCallAction)

-- | An ISO8601-formatted timestamp that indicates when the API call was
-- first observed.
awsApiCallAction_firstSeen :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_firstSeen = Lens.lens (\AwsApiCallAction' {firstSeen} -> firstSeen) (\s@AwsApiCallAction' {} a -> s {firstSeen = a} :: AwsApiCallAction)

-- | Indicates whether the API call originated from a remote IP address
-- (@remoteip@) or from a DNS domain (@domain@).
awsApiCallAction_callerType :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_callerType = Lens.lens (\AwsApiCallAction' {callerType} -> callerType) (\s@AwsApiCallAction' {} a -> s {callerType = a} :: AwsApiCallAction)

-- | Identifies the resources that were affected by the API call.
awsApiCallAction_affectedResources :: Lens.Lens' AwsApiCallAction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsApiCallAction_affectedResources = Lens.lens (\AwsApiCallAction' {affectedResources} -> affectedResources) (\s@AwsApiCallAction' {} a -> s {affectedResources = a} :: AwsApiCallAction) Prelude.. Lens.mapping Lens.coerced

-- | An ISO8601-formatted timestamp that indicates when the API call was most
-- recently observed.
awsApiCallAction_lastSeen :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_lastSeen = Lens.lens (\AwsApiCallAction' {lastSeen} -> lastSeen) (\s@AwsApiCallAction' {} a -> s {lastSeen = a} :: AwsApiCallAction)

-- | Provided if @CallerType@ is @domain@. Provides information about the DNS
-- domain that the API call originated from.
awsApiCallAction_domainDetails :: Lens.Lens' AwsApiCallAction (Prelude.Maybe AwsApiCallActionDomainDetails)
awsApiCallAction_domainDetails = Lens.lens (\AwsApiCallAction' {domainDetails} -> domainDetails) (\s@AwsApiCallAction' {} a -> s {domainDetails = a} :: AwsApiCallAction)

-- | The name of the Amazon Web Services service that the API method belongs
-- to.
awsApiCallAction_serviceName :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_serviceName = Lens.lens (\AwsApiCallAction' {serviceName} -> serviceName) (\s@AwsApiCallAction' {} a -> s {serviceName = a} :: AwsApiCallAction)

-- | The name of the API method that was issued.
awsApiCallAction_api :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_api = Lens.lens (\AwsApiCallAction' {api} -> api) (\s@AwsApiCallAction' {} a -> s {api = a} :: AwsApiCallAction)

instance Core.FromJSON AwsApiCallAction where
  parseJSON =
    Core.withObject
      "AwsApiCallAction"
      ( \x ->
          AwsApiCallAction'
            Prelude.<$> (x Core..:? "RemoteIpDetails")
            Prelude.<*> (x Core..:? "FirstSeen")
            Prelude.<*> (x Core..:? "CallerType")
            Prelude.<*> ( x Core..:? "AffectedResources"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LastSeen")
            Prelude.<*> (x Core..:? "DomainDetails")
            Prelude.<*> (x Core..:? "ServiceName")
            Prelude.<*> (x Core..:? "Api")
      )

instance Prelude.Hashable AwsApiCallAction where
  hashWithSalt salt' AwsApiCallAction' {..} =
    salt' `Prelude.hashWithSalt` api
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` domainDetails
      `Prelude.hashWithSalt` lastSeen
      `Prelude.hashWithSalt` affectedResources
      `Prelude.hashWithSalt` callerType
      `Prelude.hashWithSalt` firstSeen
      `Prelude.hashWithSalt` remoteIpDetails

instance Prelude.NFData AwsApiCallAction where
  rnf AwsApiCallAction' {..} =
    Prelude.rnf remoteIpDetails
      `Prelude.seq` Prelude.rnf api
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf domainDetails
      `Prelude.seq` Prelude.rnf lastSeen
      `Prelude.seq` Prelude.rnf affectedResources
      `Prelude.seq` Prelude.rnf callerType
      `Prelude.seq` Prelude.rnf firstSeen

instance Core.ToJSON AwsApiCallAction where
  toJSON AwsApiCallAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RemoteIpDetails" Core..=)
              Prelude.<$> remoteIpDetails,
            ("FirstSeen" Core..=) Prelude.<$> firstSeen,
            ("CallerType" Core..=) Prelude.<$> callerType,
            ("AffectedResources" Core..=)
              Prelude.<$> affectedResources,
            ("LastSeen" Core..=) Prelude.<$> lastSeen,
            ("DomainDetails" Core..=) Prelude.<$> domainDetails,
            ("ServiceName" Core..=) Prelude.<$> serviceName,
            ("Api" Core..=) Prelude.<$> api
          ]
      )
